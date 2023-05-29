module Clash.Cores.Ethernet.MAC.PreambleRemover ( preambleRemover ) where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream
import  Data.Maybe
import Clash.Cores.Ethernet.Stream ( SingleByteStream, SingleByteStreamFwd, mealyToCircuit )

data State = TooShort  (Index 8)| LongEnough
  deriving (Show, Eq, Generic, NFDataX)

preambleRemover :: HiddenClockResetEnable dom => Circuit (SingleByteStream dom) (SingleByteStream dom)
preambleRemover = mealyToCircuit machineAsFunction initialState where
  initialState = TooShort  0
  machineAsFunction :: State -> (SingleByteStreamFwd, Axi4StreamS2M) -> (State, (Axi4StreamS2M, SingleByteStreamFwd))
  machineAsFunction (TooShort n ) (inp, recvACK) =  (nextState ,(recvACK , out))
     where
        nextState
          |not $ _tready recvACK = TooShort n
          |isNothing inp =TooShort n
          | n==7  = LongEnough
          |otherwise = TooShort (n+1)

        out =Nothing

  machineAsFunction LongEnough (inp, recvACK) = case inp of    
                                                Nothing ->  ( (TooShort 0) ,(recvACK , inp))
                                                _       ->  (LongEnough ,(recvACK ,inp))
