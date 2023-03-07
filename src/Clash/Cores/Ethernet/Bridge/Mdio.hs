module Clash.Cores.Ethernet.Bridge.Mdio (MDIOResponse (..), MDIORequest (..), mdioComponent) where

import Clash.Prelude
import Data.Maybe ( isNothing )

data MDIORequest
  -- | PHY address, REG address
  = MDIORead (Unsigned 5) (Unsigned 5)
  -- | PHY address, REG address, 16-bit data
  | MDIOWrite (Unsigned 5) (Unsigned 5) (BitVector 16)
  deriving (Show, Eq, Generic, NFDataX)

data MDIOResponse
  = MDIOReadResult (BitVector 16)
  -- ^ contents of register that was read
  | MDIOWriteAck
  deriving (Show, Eq, Generic, NFDataX)

data PacketState
  = SendPreamble
  -- ^ Send 32 times a 1
  | SendSTOPPARA
  -- ^ Send the STart, OPeration, Physical Address and Register Address
  | SendTAWrite
  -- ^ Send 10
  | SendTARead
  -- ^ Wait while receiving Z0 (gives control to other side)
  | WriteD16
  -- ^ Write data to a register
  | ReadD16 (BitVector 16)
  -- ^ Receive data from a register
  | Idle
  -- ^ Waiting for the bridge to issue a new request
  deriving (Show, Eq, Generic, NFDataX)

-- PacketState, counter, (cached STOPPARA, data to write)
data State = State PacketState (Index 32) (BitVector 14, Maybe (BitVector 16))
  deriving (Show, Eq, Generic, NFDataX)

-- | State transitions in the mealy machine
nextStep :: State -> (Maybe MDIORequest, Bit) -> (State, (Maybe Bit, Maybe MDIOResponse))
-- Nothing happens
nextStep s@(State Idle _ _) (Nothing, _) = (s, (Nothing, Nothing))
-- Receive a request from the bridge
nextStep (State Idle _ _) (Just req, _)   = (nextState req, (Nothing,Nothing))
    where
        nextState (MDIORead phy reg) = State SendPreamble 0 (0b0110 ++# (pack phy) ++# (pack reg), Nothing)
        nextState (MDIOWrite phy reg dat) = State SendPreamble 0 (0b0101 ++# (pack phy) ++# (pack reg), Just dat)

-- Send Preamble
nextStep (State SendPreamble cnt d) (_, _) = (nextState, (Just 1, Nothing))
  where
    nextState
      | cnt < 31 = State SendPreamble (cnt+1) d
      | otherwise = State SendSTOPPARA 0 d

-- Send all 14 bits that are the same for both read and write requests
-- STart, OPeration, Physical Address, Register Address
nextStep (State SendSTOPPARA cnt (sendBv, dat)) (_,_) = (nextState, (Just out, Nothing))
  where
    -- MSB first!
    out = msb sendBv
    bv' = shiftL sendBv 1
    nextState
      | cnt < 13 = State SendSTOPPARA (cnt+1) (bv', dat)
      | isNothing dat = State SendTARead 0 (bv', dat)
      | otherwise = State SendTAWrite 0 (bv', dat)

-- Wait out the turn-around field for Read
nextStep (State SendTARead cnt d) (_, _) = (nextState, (Nothing, Nothing))
  where
    -- TODO: Check that other side correctly sends 0 when cnt == 1?
    nextState
      -- cnt == 0 = State SendTARead 1 req
      | cnt < 4 = State SendTARead (cnt+1) d
      | otherwise = State (ReadD16 0) 0 d

-- Receive data for Read
nextStep (State (ReadD16 bv) cnt d) (_, inp) = (nextState, (Just 0, response))
  where
    -- Receive a bit from the register we're reading
    -- Bit 15 first, bit 0 last!
    bv' = replaceBit 0 inp $ shiftL bv 1
    (response, nextState)
      | cnt >= 15 = (Just $ MDIOReadResult bv', State Idle 0 d)
      | otherwise = (Nothing, State (ReadD16 bv') (cnt+1) d)

-- Send turn-around field for Write
nextStep (State SendTAWrite cnt d) (_, _) = (nextState, (Just out, Nothing))
    where
        (out, nextState)
            | cnt == 0  = (1, State SendTAWrite 1 d)
            | otherwise = (0, State WriteD16 0 d)

-- Write data to register
nextStep (State WriteD16 _ (_, Nothing)) (_, _) = error "Impossible"
nextStep (State WriteD16 cnt (sendBv, Just dat)) (_, _) = (nextState, (Just out, response))
  where
    -- Get a bit from the register write
    -- Bit 15 first, bit 0 last!
    out = msb dat
    bv' = shiftL dat 1
    response
      | cnt == 15 = Just $ MDIOWriteAck
      | otherwise = Nothing
    nextState
      | cnt < 15 = State WriteD16 (cnt+1) (sendBv, Just bv')
      | otherwise = State Idle 0 (sendBv, Just bv')


mdioComponent
  :: HiddenClockResetEnable dom
  => KnownDomain dom
  -- | The mdc clock
  => Signal dom Bool
  -- | eth_mdio in
  -> Signal dom Bit
  -- | Receive a request from the bridge
  -> Signal dom (Maybe MDIORequest)
  -- |
  -- 1. eth_mdio out
  -- 2. Output result or ACK when we have it
  -> ( Signal dom (Maybe Bit)
     , Signal dom (Maybe MDIOResponse)
     )
mdioComponent mdc mdioIn req = unbundle
                         $ mealyMDC nextStep initialState
                         $ bundle (req, mdioIn)
  where
    initialState = State Idle 0 (0, Nothing)
    -- Custom mealy function for slowing down to MDC speeds
    -- Largely stolen from the Clash prelude
    mealyMDC f iS =
      \i -> let (s',o) = unbundle $ f <$> s <*> i
                -- s      = register clk rst en initialState s'
                mdcEn = isFalling False mdc
                s      = regEn iS mdcEn s'
            in  o
