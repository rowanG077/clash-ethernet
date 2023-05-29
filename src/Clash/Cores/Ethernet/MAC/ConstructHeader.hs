module Clash.Cores.Ethernet.MAC.ConstructHeader ( constructHeader ) where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream

import Clash.Cores.Ethernet.Stream

data State = Destination (Index 6) EthernetHeader | Source (Index 6) EthernetHeader | EtherType (Index 2) EthernetHeader | Passthrough
  deriving (Show, Eq, Generic, NFDataX)

constructHeader :: HiddenClockResetEnable dom => Circuit (TaggedSingleByteStream dom) (SingleByteStream dom)
constructHeader = mealyToCircuit machineAsFunction initialState where
  notReady = Axi4StreamS2M { _tready = False }
  initialState = Destination 0 undefined

  machineAsFunction :: State -> (TaggedSingleByteStreamFwd, Axi4StreamS2M) -> (State, (Axi4StreamS2M, SingleByteStreamFwd))
  machineAsFunction s@(Destination 0 _) (Nothing, _) = (s, (notReady, Nothing))
  machineAsFunction s@(Destination 0 _) (Just inp, recvACK) = (nextState, (notReady, out))
    where
      header = _tuser inp
      out = Just Axi4StreamM2S { _tdata = singleton $ destinationMAC header !! 0
                          , _tkeep = singleton True
                          , _tstrb = singleton False
                          , _tlast = False
                          , _tuser = ()
                          , _tid = 0
                          , _tdest = 0
                          }
      nextState
        | _tready recvACK = Destination 1 header
        | otherwise = s

  -- Pump out the rest of the destination address
  machineAsFunction s@(Destination n header) (_, recvACK) = (nextState, (notReady, out))
    where
      out = Just Axi4StreamM2S { _tdata = singleton $ destinationMAC header !! n
                          , _tkeep = singleton True
                          , _tstrb = singleton False
                          , _tlast = False
                          , _tuser = ()
                          , _tid = 0
                          , _tdest = 0
                          }
      nextState
        | not $ _tready recvACK = s
        | n == 5 = Source 0 header
        | otherwise = Destination (n+1) header

  -- Pump out the source address
  machineAsFunction s@(Source n header) (_, recvACK) = (nextState, (notReady, out))
    where
      out = Just Axi4StreamM2S { _tdata = singleton $ sourceMAC header !! n
                          , _tkeep = singleton True
                          , _tstrb = singleton False
                          , _tlast = False
                          , _tuser = ()
                          , _tid = 0
                          , _tdest = 0
                          }
      nextState
        | not $ _tready recvACK = s
        | n == 5 = EtherType 0 header
        | otherwise = Source (n+1) header

  -- Pump out the ethertype
  machineAsFunction s@(EtherType n header) (_, recvACK) = (nextState, (notReady, out))
    where
      vecEtherType :: Vec 2 (Unsigned 8)
      vecEtherType = unpack $ etherType header
      out = Just Axi4StreamM2S { _tdata = singleton $ vecEtherType !! n
                          , _tkeep = singleton True
                          , _tstrb = singleton False
                          , _tlast = False
                          , _tuser = ()
                          , _tid = 0
                          , _tdest = 0
                          }
      nextState
        | not $ _tready recvACK = s
        | n == 1 = Passthrough
        | otherwise = EtherType (n+1) header

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
