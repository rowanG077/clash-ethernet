module Clash.Cores.Ethernet.MAC.PreambleRemover ( preambleRemover ) where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream

import Clash.Cores.Ethernet.Stream ( SingleByteStream, SingleByteStreamFwd, mealyToCircuit )

data State = Idle Int | NextState Int
  deriving (Show, Eq, Generic, NFDataX)

preambleRemover :: HiddenClockResetEnable dom => Circuit (SingleByteStream dom) (SingleByteStream dom)
preambleRemover = mealyToCircuit machineAsFunction initialState where
  initialState = Idle 0
  machineAsFunction :: State -> (SingleByteStreamFwd, Axi4StreamS2M) -> (State, (Axi4StreamS2M, SingleByteStreamFwd))
  machineAsFunction (Idle n) (inp, recvACK) =  (nextState ,(recvACK , out))
     where
        counter 
          | isNothing inp = 0
          | _tready recvACK = n+1
          | otherwise = n
        nextState
          | n==8  = NextState (n-8)
          |otherwise = Idle n
        out
          | n < 8 = Nothing
           -- Discard the preamble bytes
          | otherwise = inp 
