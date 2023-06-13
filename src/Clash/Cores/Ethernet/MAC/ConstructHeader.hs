module Clash.Cores.Ethernet.MAC.ConstructHeader ( constructHeader ) where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream

import Clash.Cores.Ethernet.Stream

data State = Outputting (Index 14) (Vec 14 (Unsigned 8)) | Passthrough
  deriving (Show, Eq, Generic, NFDataX)

constructHeader :: HiddenClockResetEnable dom => Circuit (TaggedStream dom) (FourByteStream dom)
constructHeader = mealyToCircuit machineAsFunction initialState where
  notReady = Axi4StreamS2M { _tready = False }
  initialState = Outputting 0 (replicate d14 0)

  machineAsFunction :: State -> (TaggedStreamFwd, Axi4StreamS2M) -> (State, (Axi4StreamS2M, FourByteStreamFwd))
  -- Get the header from the first Axi4 transmission
  machineAsFunction s@(Outputting 0 _) (Nothing, _) = (s, (notReady, Nothing))
  machineAsFunction s@(Outputting 0 _) (Just inp, recvACK) = (nextState, (notReady, out))
    where
      header = _tuser inp
      buf = unpack $ pack (destinationMAC header, sourceMAC header, etherType header)
      out = Just Axi4StreamM2S { _tdata = takeI buf
                          , _tkeep = replicate d4 True
                          , _tstrb = replicate d4 False
                          , _tlast = False
                          , _tuser = ()
                          , _tid = 0
                          , _tdest = 0
                          }
      nextState
        | _tready recvACK = Outputting 4 (rotateLeftS buf d4)
        | otherwise = s

  -- Pump out the rest of the header
  machineAsFunction s@(Outputting n header) (_, recvACK) = (nextState, (notReady, out))
    where
      newCount = boundedAdd n 4
      out = Just Axi4StreamM2S { _tdata = takeI header
                          , _tkeep = map (\x -> n <= maxBound - x) $ generate d4 (+1) 0
                          , _tstrb = replicate d4 False
                          , _tlast = False
                          , _tuser = ()
                          , _tid = 0
                          , _tdest = 0
                          }
      nextState
        | not $ _tready recvACK = s
        | newCount >= maxBound = Passthrough
        | otherwise = Outputting newCount (rotateLeftS header d4)

  -- Passthrough mode, just append the rest of the packet after the header
  machineAsFunction Passthrough (Nothing, recvACK) = (Passthrough, (recvACK, Nothing))
  machineAsFunction Passthrough (Just inp, recvACK) = (nextState, (recvACK, out)) where
      nextState
        | _tlast inp && _tready recvACK = initialState
        | otherwise = Passthrough
      out = Just Axi4StreamM2S { _tdata = _tdata inp
                          , _tkeep = _tkeep inp
                          , _tstrb = _tstrb inp
                          , _tlast = _tlast inp
                          , _tuser = ()
                          , _tid = _tid inp
                          , _tdest = _tdest inp
                          }
