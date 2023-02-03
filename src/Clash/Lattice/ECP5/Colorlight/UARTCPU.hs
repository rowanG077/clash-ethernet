{-# language FlexibleContexts #-}

module Clash.Lattice.ECP5.Colorlight.UARTCPU where

import Clash.Cores.UART
import Clash.Explicit.Prelude
import Clash.Prelude ( exposeClockResetEnable )

data Instruction
  = InstructionAdd
  | InstructionSub
  | InstructionXor
  | InstructionNoOp
  deriving (Show, Eq, Generic, NFDataX)

decodeInstr :: BitVector 8 -> Instruction
decodeInstr 0 = InstructionAdd
decodeInstr 1 = InstructionSub
decodeInstr 2 = InstructionXor
decodeInstr _ = InstructionNoOp

data RegisterFile = RegisterFile (Unsigned 8) (Unsigned 8)
  deriving (Show, Eq, Generic, NFDataX)

data CPUOpState
  = CPUIdle
  | CPUAwaitRegA Instruction
  | CPUAwaitRegB Instruction
  | CPURunInstr Instruction
  | CPUWriteResult (BitVector 8)
  deriving (Show, Eq, Generic, NFDataX)

data CPUState = CPUState CPUOpState RegisterFile
  deriving (Show, Eq, Generic, NFDataX)

stepCPU :: CPUState -> (Maybe (BitVector 8), Bool) -> (CPUState, Maybe (BitVector 8))
stepCPU s@(CPUState CPUIdle _) (Nothing, _) = (s, Nothing)
stepCPU (CPUState CPUIdle regFile) (Just bv, _)
  = case (decodeInstr bv) of
      InstructionNoOp  -> (CPUState CPUIdle regFile, Nothing)
      i                -> (CPUState (CPUAwaitRegA i) regFile, Nothing)

stepCPU s@(CPUState (CPUAwaitRegA _) _) (Nothing, _) = (s, Nothing)
stepCPU (CPUState (CPUAwaitRegA i) (RegisterFile _ b)) (Just bv, _) = (nextState, Nothing)
  where
    nextState = CPUState (CPUAwaitRegB i) (RegisterFile (unpack bv) b)

stepCPU s@(CPUState (CPUAwaitRegB _) _) (Nothing, _) = (s, Nothing)
stepCPU (CPUState (CPUAwaitRegB i) (RegisterFile a _)) (Just bv, _) = (nextState, Nothing)
  where
    nextState = CPUState (CPURunInstr i) (RegisterFile a (unpack bv))

stepCPU (CPUState (CPURunInstr i) r@(RegisterFile a b)) _ = (nextState, Nothing)
  where
    nextState = CPUState (CPUWriteResult (pack result)) r
    result = case i of
               InstructionAdd -> a + b
               InstructionSub -> a - b
               InstructionXor -> xor a b
               _              -> error "Impossible!"

stepCPU (CPUState s@(CPUWriteResult bv) r) (_, ack) = (nextState, Just bv)
  where
    nextState | ack       = CPUState CPUIdle r
              | otherwise = CPUState s r

uartCPU
  :: forall (dom :: Domain) (baud :: Nat)
   . ValidBaud dom baud
  => KnownDomain dom
  => SNat baud
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom Bit
  -> Signal dom Bit
uartCPU baud clk rst en uartRxBit = uartTxBit
  where
    uart' = exposeClockResetEnable uart clk rst en baud
    (rxWord, uartTxBit, ack) = uart' uartRxBit txReq

    initialState = CPUState CPUIdle (RegisterFile 0 0)
    input = bundle (rxWord, ack)
    txReq = mealy clk rst en stepCPU initialState input
