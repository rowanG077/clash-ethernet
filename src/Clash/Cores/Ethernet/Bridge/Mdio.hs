module Clash.Cores.Ethernet.Bridge.Mdio (MDIOResponse (..), MDIORequest (..), mdioComponent) where

import Clash.Prelude

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
  | SendST
  -- ^ Send 01
  | SendOP
  -- ^ Send 10 for read, 01 for write
  | SendPA5
  -- ^ Send the PHY address
  | SendRA5
  -- ^ Send the REG address
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

-- PacketState, counter, MDIORequest
data State = State PacketState (Index 32) MDIORequest
  deriving (Show, Eq, Generic, NFDataX)

-- | Send the two bits in this order and then transition into a new `PacketState`
sendTwo :: PacketState -> (Bit,Bit) -> State -> (State, (Maybe Bit, Maybe MDIOResponse))
sendTwo nextStage (fstBit, sndBit) (State curState cnt req) = (nextState, (Just out, Nothing))
  where
    out
      | cnt == 0 = fstBit
      | otherwise = sndBit
    nextState
      | cnt == 0 = State curState 1 req
      | otherwise = State nextStage 0 req

-- | Send an address and then transition into a new `PacketState`
sendFive :: PacketState -> (Unsigned 5) -> State -> (State, (Maybe Bit, Maybe MDIOResponse))
sendFive nextStage bv (State curState cnt req) = (nextState, (Just out, Nothing))
  where
    -- Get the `cnt`-th bit of the address
    -- MSB first!
    out = bv ! (5 - cnt)
    nextState
      | cnt < 4 = State curState (cnt+1) req
      | otherwise = State nextStage 0 req

-- | State transitions in the mealy machine
nextStep :: State -> (Maybe MDIORequest, Bit) -> (State, (Maybe Bit, Maybe MDIOResponse))
-- Nothing happens
nextStep s@(State Idle _ _) (Nothing, _) = (s, (Nothing, Nothing))
-- Receive a request from the bridge
nextStep (State Idle _ _) (Just req, _)   = (State SendPreamble 0 req, (Nothing,Nothing))

-- Send Preamble
nextStep (State SendPreamble cnt req) (_, _) = (nextState, (Just 1, Nothing))
  where
    nextState
      | cnt < 31 = State SendPreamble (cnt+1) req
      | otherwise = State SendST 0 req

-- Send ST
nextStep s@(State SendST _ _) (_, _) = sendTwo SendOP (0,1) s

-- Send OP
nextStep s@(State SendOP _ req) (_, _) = sendTwo SendPA5 (op req) s
  where
    -- Output 10
    op (MDIORead _ _) = (1,0)
    -- Output 01
    op (MDIOWrite _ _ _) = (0,1)

-- Send PA5
nextStep s@(State SendPA5 _ req) (_, _) = sendFive SendRA5 (phyAddr req) s
  where
    -- Get the PHY address
    phyAddr :: MDIORequest -> (Unsigned 5)
    phyAddr (MDIORead addr _) = addr
    phyAddr (MDIOWrite addr _ _) = addr

-- Send RA5
nextStep s@(State SendRA5 _ req) (_, _) = sendFive (nextStage req) (regAddr req) s
  where
    nextStage (MDIORead _ _) = SendTARead
    nextStage (MDIOWrite _ _ _) = SendTAWrite
    -- Get the REG address
    regAddr :: MDIORequest -> (Unsigned 5)
    regAddr (MDIORead _ addr) = addr
    regAddr (MDIOWrite _ addr _) = addr

-- Wait out the turn-around field for Read
nextStep (State SendTARead cnt req) (_, _) = (nextState, (Nothing, Nothing))
  where
    -- TODO: Check that other side correctly sends 0 when cnt == 1?
    nextState
      | cnt == 0 = State SendTARead 1 req
      | otherwise = State (ReadD16 0) 0 req

-- Receive data for Read
nextStep (State (ReadD16 bv) cnt req) (_, inp) = (nextState, (Nothing, response))
  where
    -- Receive a bit from the register we're reading
    -- Bit 15 first, bit 0 last!
    bv' = replaceBit (15-cnt) inp bv
    response
      | cnt == 15 = Just $ MDIOReadResult bv'
      | otherwise = Nothing
    nextState
      | cnt < 15 = State (ReadD16 bv') (cnt+1) req
      -- TODO: Make a new blank `req` here?
      | otherwise = State Idle 0 req

-- Send turn-around field for Write
nextStep s@(State SendTAWrite _ _) (_, _) = sendTwo WriteD16 (1,0) s

-- Write data to register
nextStep (State WriteD16 cnt req) (_, _) = (nextState, (Just out, response))
  where
    out = case req of
      (MDIORead _ _) -> error "Impossible"
                            -- Get a bit from the register write
                            -- Bit 15 first, bit 0 last!
      (MDIOWrite _ _ bv) -> bv ! (15 - cnt)
    response
      | cnt == 15 = Just $ MDIOWriteAck
      | otherwise = Nothing
    nextState
      | cnt < 15 = State WriteD16 (cnt+1) req
      -- TODO: Make a new blank `req` here?
      | otherwise = State Idle 0 req

mdioComponent
  :: HiddenClockResetEnable dom
  => KnownDomain dom
  -- | eth_mdio in
  => Signal dom Bit
  -- | Receive a request from the bridge
  -> Signal dom (Maybe MDIORequest)
  -- |
  -- 1. eth_mdio out
  -- 2. Output result or ACK when we have it
  -> ( Signal dom (Maybe Bit)
     , Signal dom (Maybe MDIOResponse)
     )
mdioComponent mdioIn req = unbundle
                         $ mealy nextStep initialState
                         $ bundle (req, mdioIn)
  where
    initialState = State Idle 0 (MDIORead 0 0)
