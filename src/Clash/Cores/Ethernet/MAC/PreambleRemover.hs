module Clash.Cores.Ethernet.MAC.PreambleRemover ( preambleRemover ) where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream

import Clash.Cores.Ethernet.Stream ( SingleByteStream, SingleByteStreamFwd, mealyToCircuit )

data State = Idle
  deriving (Show, Eq, Generic, NFDataX)

preambleRemover :: HiddenClockResetEnable dom => Circuit (SingleByteStream dom) (SingleByteStream dom)
preambleRemover = mealyToCircuit machineAsFunction initialState where
  initialState = undefined
  machineAsFunction :: State -> (SingleByteStreamFwd, Axi4StreamS2M) -> (State, (Axi4StreamS2M, SingleByteStreamFwd))
  -- TODO: Actually implement this:
  machineAsFunction s (inp, recvACK) = (s, (recvACK, inp))
