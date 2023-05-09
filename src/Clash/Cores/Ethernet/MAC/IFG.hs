module Clash.Cores.Ethernet.MAC.IFG ( ifgEnforcer ) where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream

import Clash.Cores.Ethernet.Stream ( mealyToCircuit, SingleByteStream, SingleByteStreamFwd )


ifgEnforcer :: HiddenClockResetEnable dom => Circuit (SingleByteStream dom) (SingleByteStream dom)
ifgEnforcer = mealyToCircuit machineAsFunction 0 where
  notReady = Axi4StreamS2M { _tready = False }

  machineAsFunction :: Index 13 -> (SingleByteStreamFwd, Axi4StreamS2M) -> (Index 13, (Axi4StreamS2M, SingleByteStreamFwd))
  machineAsFunction 0 (inp, recvACK) = (newCounter, (recvACK, inp))
    where
      -- If this was the last byte of the frame, then skip 12 cycles for the Inter Frame Gap
      newCounter = if maybe False _tlast inp then 12 else 0
  machineAsFunction n (_, _) = (n-1, (notReady, Nothing))
