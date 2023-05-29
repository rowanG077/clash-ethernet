module Clash.Cores.Ethernet.MAC.DeconstructHeader ( deconstructHeader ) where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream

import Clash.Cores.Ethernet.Stream
    ( EthernetHeader(..), SingleByteStream, SingleByteStreamFwd, TaggedSingleByteStream,
    TaggedSingleByteStreamFwd, mealyToCircuit )

data State = Idle
  deriving (Show, Eq, Generic, NFDataX)

deconstructHeader :: HiddenClockResetEnable dom => Circuit (SingleByteStream dom) (TaggedSingleByteStream dom)
deconstructHeader = mealyToCircuit machineAsFunction initialState where
  initialState = undefined
  machineAsFunction :: State -> (SingleByteStreamFwd, Axi4StreamS2M) -> (State, (Axi4StreamS2M, TaggedSingleByteStreamFwd))
  -- TODO: Actually implement this:
  machineAsFunction s (Nothing, recvACK) = (s, (recvACK, Nothing))
  machineAsFunction s (Just inp, recvACK) = (s, (recvACK, Just out))
    where
      out = Axi4StreamM2S { _tdata = _tdata inp
                          , _tkeep = _tkeep inp
                          , _tstrb = _tstrb inp
                          , _tlast = _tlast inp
                          -- Example Ethernet header, see Stream.hs
                          , _tuser = EthernetHeader { destinationMAC = replicate d6 255
                                                    , sourceMAC = replicate d6 0
                                                    , etherType = 0x0800
                                                    }
                          , _tid = _tid inp
                          , _tdest = _tdest inp
                          }
