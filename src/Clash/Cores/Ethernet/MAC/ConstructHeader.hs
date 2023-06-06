module Clash.Cores.Ethernet.MAC.ConstructHeader ( constructHeader ) where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream

import Clash.Cores.Ethernet.Stream

data State = Outputting (Index 14) (Vec 14 (Unsigned 8)) | Passthrough
  deriving (Show, Eq, Generic, NFDataX)

constructHeader :: HiddenClockResetEnable dom => Circuit (TaggedSingleByteStream dom) (SingleByteStream dom)
constructHeader = mealyToCircuit machineAsFunction initialState where
  notReady = Axi4StreamS2M { _tready = False }
  initialState = Outputting 0 (replicate d14 0)

  machineAsFunction :: State -> (TaggedSingleByteStreamFwd, Axi4StreamS2M) -> (State, (Axi4StreamS2M, SingleByteStreamFwd))
  -- Get the header from the first Axi4 transmission
  machineAsFunction s@(Outputting 0 _) (Nothing, _) = (s, (notReady, Nothing))
  machineAsFunction s@(Outputting 0 _) (Just inp, recvACK) = (nextState, (notReady, out))
    where
      header = _tuser inp
      buf = unpack $ pack (destinationMAC header, sourceMAC header, etherType header)
      out = Just Axi4StreamM2S { _tdata = singleton $ head $ buf
                          , _tkeep = singleton True
                          , _tstrb = singleton False
                          , _tlast = False
                          , _tuser = ()
                          , _tid = 0
                          , _tdest = 0
                          }
      nextState
        | _tready recvACK = Outputting 1 buf
        | otherwise = s

  -- Pump out the rest of the header
  machineAsFunction s@(Outputting n header) (_, recvACK) = (nextState, (notReady, out))
    where
      out = Just Axi4StreamM2S { _tdata = singleton $ header !! n
                          , _tkeep = singleton True
                          , _tstrb = singleton False
                          , _tlast = False
                          , _tuser = ()
                          , _tid = 0
                          , _tdest = 0
                          }
      nextState
        | not $ _tready recvACK = s
        | n == 13 = Passthrough
        | otherwise = Outputting (n+1) header

  -- Passthrough mode, just append the rest of the packet after the header
  machineAsFunction Passthrough (Nothing, recvACK) = (Passthrough, (recvACK, Nothing))
  machineAsFunction Passthrough (Just inp, recvACK) = (nextState, (recvACK, out)) where
      nextState
        | _tlast inp = initialState
        | otherwise = Passthrough
      out = Just Axi4StreamM2S { _tdata = _tdata inp
                          , _tkeep = _tkeep inp
                          , _tstrb = _tstrb inp
                          , _tlast = _tlast inp
                          , _tuser = ()
                          , _tid = _tid inp
                          , _tdest = _tdest inp
                          }
