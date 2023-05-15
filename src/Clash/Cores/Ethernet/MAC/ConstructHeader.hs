module Clash.Cores.Ethernet.MAC.ConstructHeader ( constructHeader ) where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream

import Clash.Cores.Ethernet.Stream ( mealyToCircuit, SingleByteStream, SingleByteStreamFwd, TaggedSingleByteStream, TaggedSingleByteStreamFwd )

data State = Idle
  deriving (Show, Eq, Generic, NFDataX)

constructHeader :: HiddenClockResetEnable dom => Circuit (TaggedSingleByteStream dom) (SingleByteStream dom)
constructHeader = mealyToCircuit machineAsFunction initialState where
  initialState = undefined
  machineAsFunction :: State -> (TaggedSingleByteStreamFwd, Axi4StreamS2M) -> (State, (Axi4StreamS2M, SingleByteStreamFwd))
  -- TODO: Actually implement this:
  machineAsFunction s (Nothing, recvACK) = (s, (recvACK, Nothing))
  machineAsFunction s (Just inp, recvACK) = (s, (recvACK, Just out))
    where
      out = Axi4StreamM2S { _tdata = _tdata inp
                          , _tkeep = _tkeep inp
                          , _tstrb = _tstrb inp
                          , _tlast = _tlast inp
                          , _tuser = ()
                          , _tid = _tid inp
                          , _tdest = _tdest inp
                          }
