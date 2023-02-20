{-# language FlexibleContexts #-}

module Clash.Cores.Ethernet.Bridge.Bridge (uartCPU) where

import Clash.Cores.Ethernet.Bridge.Mdio
import Clash.Prelude
import Clash.Cores.UART

uartToMdioBridge :: HiddenClockResetEnable dom
  => KnownDomain dom
  => Signal dom (Maybe (BitVector 8)) -- receive byte from UART
  -> Signal dom Bool -- Ack coming from UART
  -> Signal dom (Maybe MDIOResponse) -- get result of request from mdio component
  -> (Signal dom (Maybe (BitVector 8)) -- transmit byte to UART
  , Signal dom (Maybe MDIORequest)) -- request to mdio component
uartToMdioBridge _ _ _ = error "unimplemented"

uartCPU :: forall (dom :: Domain) (baud :: Nat) . ValidBaud dom baud
  => KnownDomain dom
  => SNat baud
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom Bit -- uart rx pin
  -> Signal dom (Maybe MDIOResponse) -- get result of request from mdio component
  -> (Signal dom Bit -- uart tx pin
  , Signal dom (Maybe MDIORequest)) -- output request to mdio component
uartCPU baud clk rst en uartRxBit mdioResponse = (uartTxBit, mdioRequest)
  where
    uart' = exposeClockResetEnable uart clk rst en baud
    (rxWord, uartTxBit, ack) = uart' uartRxBit txReq

    bridgeWithClockResetEnable = exposeClockResetEnable uartToMdioBridge
    txReq :: Signal dom (Maybe (BitVector 8))
    mdioRequest :: Signal dom (Maybe MDIORequest)
    (txReq, mdioRequest) = bridgeWithClockResetEnable clk rst en rxWord ack mdioResponse
