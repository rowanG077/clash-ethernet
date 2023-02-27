{-# language FlexibleContexts #-}

module Clash.Cores.Ethernet.Bridge.Bridge (uartCPU) where

import Clash.Cores.Ethernet.Bridge.Mdio
import Clash.Prelude
import Clash.Cores.UART


data Instruction = UARTRead | UARTWrite | UARTNoOp
  deriving (Show, Eq, Generic, NFDataX)

decodeUARTInstr :: BitVector 8 -> Instruction
decodeUARTInstr 0 = UARTRead
decodeUARTInstr 1 = UARTWrite
decodeUARTInstr _ = UARTNoOp

-- | datatype that stores all the registers
data BridgeRegisters = BridgeRegisters
    { physicalAddr :: Unsigned 5,
      registerAddr :: Unsigned 5,
      d16content :: Unsigned 16
    } deriving (Show, Eq, Generic, NFDataX)

data BridgeOpState
  --  read/write common states
  = Idle
  | AwaitPhyAddr Instruction 
  | AwaitRegAddr Instruction
  | AwaitD16 Int Instruction
  | SendInstruction Instruction
  -- read specific states
  | AwaitMDIOResponse
  | ProcessResponseToUART1
  | WriteResponseToUART1 (BitVector 8)
  | ProcessResponseToUART2
  | WriteResponseToUART2 (BitVector 8)
  deriving (Show, Eq, Generic, NFDataX)

data BridgeState = BridgeState BridgeOpState BridgeRegisters
  deriving (Show, Eq, Generic, NFDataX)

type UARTInput = (Maybe (BitVector 8), Bool)
type UARTOutput = (BitVector 8)

bridgeStep :: BridgeState 
  -> (UARTInput, Maybe MDIOResponse)
  -> (BridgeState, (Maybe UARTOutput, Maybe MDIORequest))

{- read and write common states -> no case distinctions on instruction -}
-- > idle state
bridgeStep s@(BridgeState Idle registers) ((input, _), _)
  = case input of
      Nothing -> (s, noOutput)
      Just bv -> (, noOutput) . instructionToState . decodeUARTInstr $ bv
    where
      -- stay in same state if noop
      instructionToState UARTNoOp = s 
      -- go to await physical address in all other cases
      instructionToState instr = BridgeState (AwaitPhyAddr instr) registers
      noOutput = (Nothing, Nothing)

-- > waiting for physical device address
bridgeStep (BridgeState (AwaitPhyAddr instr) registers) ((Just bv, _), _)
  = (nextState, noOutput)
    where
      nextState = (BridgeState (AwaitRegAddr instr) newRegs)
      newRegs = registers { physicalAddr = resize . unpack $ bv }
      noOutput = (Nothing, Nothing)

-- > waiting for mdio register address
bridgeStep (BridgeState (AwaitRegAddr instr) registers) ((Just bv, _), _)
  = (nextState, noOutput)
    where
      nextState = (BridgeState (AwaitD16 0 instr) newRegs)
      newRegs = registers { registerAddr = resize . unpack $ bv }
      noOutput = (Nothing, Nothing)

-- > waiting for d16 data
bridgeStep (BridgeState (AwaitD16 cnt instr) registers) ((Just input, _), _)
  = (nextState, noOutput)
    where
      -- no output
      noOutput = (Nothing, Nothing)
      -- write the output into d16 register
      newRegs = 
        case cnt of 
          -- put first bits into msb slot
          0 -> registers { d16content = (\x -> shiftL x 8) . resize . unpack $ input }
          -- put second bits into lsb slot
          1 -> registers { d16content = (xor $ d16content registers) . resize . unpack $ input }
          _ -> error "Impossible"
      -- next we send the request
      nextState = 
        case cnt of 
          -- wait for next bit packet
          0 -> BridgeState (AwaitD16 1 instr) newRegs
          -- sent bits to mdio component
          1 -> BridgeState (SendInstruction instr) newRegs
          _ -> error "Impossible"

{- read specific states -}
-- > sending the read instruction to MDIO
bridgeStep (BridgeState (SendInstruction UARTRead) registers) _
  = (nextState, (requestToUart, requestToMdio))
    where
      requestToUart = Nothing
      -- create MDIORequest i.e. retrive data that is stored at the specified address
      requestToMdio = pure $ MDIORead (physicalAddr registers) (registerAddr registers)
      -- next we wait for a reponse
      nextState = BridgeState AwaitMDIOResponse registers

-- > receiving MDIO reponse and put it into a register
bridgeStep (BridgeState AwaitMDIOResponse registers) (_, Just input)
  = (nextState, noOutput)
    where      
      -- no ouput is sent
      noOutput = (Nothing, Nothing)
      -- move response into register
      newRegs = case input of 
                  MDIOReadResult r -> registers { d16content = unpack r }
                  _ -> error "Impossible"
      -- next we wait for a reponse
      nextState = BridgeState ProcessResponseToUART1 newRegs

-- > processing result -> get 8 msb of result
bridgeStep (BridgeState ProcessResponseToUART1 registers) (_, _)
  = (nextState, noOutput)
    where
      -- no ouput is sent
      noOutput = (Nothing, Nothing)
      -- create bitvector
      bv = pack . resize . (\x -> shiftR x 8) . d16content $ 7 --registers
      -- next we wait for a reponse
      nextState = BridgeState (WriteResponseToUART1 bv) registers

-- > write to uart -> wait for ack
bridgeStep (BridgeState s@(WriteResponseToUART1 bv) registers) ((_, ack), _)
  = (nextState, (requestToUart, requestToMdio))
    where
      -- write first 8 most significant bits to uart
      requestToUart = pure bv
      requestToMdio = Nothing
      -- if ack is recieved we send next 8 bits. Otherwise we stay in same state
      nextState | ack       = BridgeState ProcessResponseToUART2 registers
                | otherwise = BridgeState s registers

-- > processing second part of response
bridgeStep (BridgeState ProcessResponseToUART2 registers) (_, _)
  = (nextState, noOutput)
    where
      -- no ouput is sent
      noOutput = (Nothing, Nothing)
      -- create bitvector
      bv = pack . resize . d16content 7 --registers
      -- next we send the bits
      nextState = BridgeState (WriteResponseToUART2 bv) registers

-- > write to uart second part -> wait for ack
bridgeStep (BridgeState s@(WriteResponseToUART2 bv) registers) ((_, ack), _)
  = (nextState, (requestToUart, requestToMdio))
    where
      -- write first 8 least significant bits to uart
      requestToUart = pure bv
      requestToMdio = Nothing
      -- if ack is recieved we go back to idle. Otherwise we stay in same state
      nextState | ack       = BridgeState Idle registers
                | otherwise = BridgeState s registers

{- write specific states -}
-- > sending the read instruction to MDIO
bridgeStep (BridgeState s@(SendInstruction UARTWrite) registers) (_, repl)
  = (nextState, (requestToUart, requestToMdio))
    where
      requestToUart = Nothing
      -- create MDIORequest i.e. retrive data that is stored at the specified address
      requestToMdio = case repl of
                        Just MDIOWriteAck -> Nothing
                        _ ->    pure $ MDIOWrite (physicalAddr registers) 
                                                 (registerAddr registers) 
                                                 (pack . d16content $ registers)
      -- next we wait for a reponse -> if response received go back to idel state
      nextState = case repl of 
                    Just MDIOWriteAck -> BridgeState Idle registers
                    _ -> BridgeState s registers

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
    initState = BridgeState Idle (BridgeRegisters 0 0 0)

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
