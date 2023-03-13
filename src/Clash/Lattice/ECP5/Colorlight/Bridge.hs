{-# language FlexibleContexts #-}

module Clash.Lattice.ECP5.Colorlight.Bridge (uartCPU) where

import Clash.Cores.Ethernet.Bridge.Mdio
import Clash.Prelude
import Clash.Cores.UART


data Instruction = UARTRead | UARTWrite | UARTNoOp
  deriving (Show, Eq, Generic, NFDataX)

-- functions for instruction decoding
decodeUARTInstr :: BitVector 8 -> Instruction
decodeUARTInstr 0 = UARTRead
decodeUARTInstr 1 = UARTWrite
decodeUARTInstr _ = UARTNoOp

data BridgeState
  --  read/write common states
  = Idle
  | UARTReceive (Index 5) (Vec 5 (BitVector 8))
  | MDIOAwaitResponse MDIORequest
  | UARTSend (Index 2) (Vec 2 (BitVector 8))
  deriving (Show, Eq, Generic, NFDataX)

type UARTInput = (Maybe (BitVector 8), Bool)
type UARTOutput = (BitVector 8)

bridgeStep :: BridgeState 
  -> (UARTInput, Maybe MDIOResponse)
  -> (BridgeState, (Maybe UARTOutput, Maybe MDIORequest))

{- Idle -}
bridgeStep Idle ((Nothing, _), _)
  = (Idle, noOutput)
    where
      -- state does not produce output
      noOutput = (Nothing, Nothing)

bridgeStep Idle ((Just bv, _), _)
  = (nextState, noOutput)
    where
      -- state does not produce output
      noOutput = (Nothing, Nothing)
      -- initialize regs and state
      regs = bv +>> repeat 0
      nextState = UARTReceive 3 regs

{- Receiving data from UART -}
bridgeStep (UARTReceive idx regs) ((Nothing, _), _)
  = (nextState, noOutput)
  where
    -- state does not produce output
    noOutput = (Nothing, Nothing)
    nextState = UARTReceive idx regs

bridgeStep (UARTReceive idx regs) ((Just bv, _), _)
  | idx > 0 = (,noOutput) $ UARTReceive (idx - 1) regs'
  | idx == 0 = (nextState, noOutput) 
  | idx < 0 = error "Impossible"
  where
    -- state does not produce output
    noOutput = (Nothing, Nothing)
    -- update regs
    regs' = (bv +>> regs)
    -- decode the uart insturction
    instruction = decodeUARTInstr $ regs' !! (4 :: Index 5)
    -- request builder
    phyaddr = unpack . resize $ regs' !! (3 :: Index 5)
    regaddr = unpack . resize $ regs' !! (2 :: Index 5)
    d16data = unpack (regs' !! (1 :: Index 5)) ++# (regs' !! (0 :: Index 5))
    readReq = MDIORead phyaddr regaddr
    writeReq = MDIOWrite phyaddr regaddr d16data
    -- go to next state
    nextState = case instruction of
                  UARTRead -> MDIOAwaitResponse readReq
                  UARTWrite -> MDIOAwaitResponse writeReq
                  _ -> Idle

{- Keep sending data to MDIO until response -}
bridgeStep s@(MDIOAwaitResponse req) (_, Nothing)
  = (s, (Nothing, pure req))

bridgeStep (MDIOAwaitResponse _) (_, Just MDIOWriteAck)
  = (Idle, (Nothing, Nothing))

bridgeStep (MDIOAwaitResponse _) (_, Just (MDIOReadResult bv))
  = (nextState, (Nothing, Nothing))
    where
      -- next we send data to uart
      nextState = UARTSend 1 regs
      -- initialize regs
      regs = bv1 :> bv2 :> Nil
      (bv1, bv2) = split bv

{- Keep sending data to MDIO until acked -}
bridgeStep s@(UARTSend idx regs) ((_, False), _)
  = (s, (uartOutput, Nothing))
  where
    uartOutput = pure $ regs !! idx

bridgeStep (UARTSend idx regs) ((_, True), _)
  | idx > 0 = (nextState, (uartOutput, Nothing))
  | idx == 0 = (Idle, noOutput)
  where
    nextState = UARTSend (idx-1) regs
    uartOutput = pure $ regs !! (idx-1)
    noOutput = (Nothing, Nothing)

uartToMdioBridge :: HiddenClockResetEnable dom
  => KnownDomain dom
  => Signal dom (Maybe (BitVector 8)) 
  -- ^ receive byte from UART
  -> Signal dom Bool 
  -- ^ Ack coming from UART
  -> Signal dom (Maybe MDIOResponse) 
  -- ^ get result of request from mdio component
  -> ( Signal dom (Maybe (BitVector 8)) 
  -- ^ transmit byte to UART
     , Signal dom (Maybe MDIORequest)) 
  -- ^ request to mdio component
uartToMdioBridge rxByte ackRx mdioResp = (txByte, mdioReq)
  where
    -- define state signal
    initState = Idle

    -- define output signal
    outputSignal = mealy  bridgeStep initState $ bundle (bundle (rxByte, ackRx), mdioResp)

    -- define output signals
    (txByte, mdioReq) = unbundle outputSignal

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
