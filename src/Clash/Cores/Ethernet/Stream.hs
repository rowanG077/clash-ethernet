{-# LANGUAGE NumericUnderscores #-}

module Clash.Cores.Ethernet.Stream (streamTestFramePerSecond, ifgEnforcer, preambleInserter, withCircuit) where

import qualified Clash.Prelude as C
import Clash.Prelude
import Protocols
import Protocols.Df
import Protocols.Axi4.Stream
import Clash.Cores.Ethernet.Frame (testFrame)

-- TODO: Clean up and sort the `type`s here into something comprehensible

type Byte = Unsigned 8

-- | Custom metadata for a stream
type UserType = ()

-- | The config for our Axi4Stream
type AxiStream (dom :: Domain) = Axi4Stream dom ('Axi4StreamConfig 4 0 0) UserType
type AxiStreamFwd = Maybe (Axi4StreamM2S ('Axi4StreamConfig 4 0 0) UserType)
type AxiStreamBwd = Axi4StreamS2M

type AxiSingleStream (dom :: Domain) = Axi4Stream dom ('Axi4StreamConfig 1 0 0) UserType
type AxiSingleStreamFwd = Maybe (Axi4StreamM2S ('Axi4StreamConfig 1 0 0) UserType)

-- | The config for an Axi4Stream tagged with an eth_type as destination
type AxiConfigTagged = 'Axi4StreamConfig 4 0 16

withCircuit :: KnownDomain dom => Circuit (AxiSingleStream dom) (AxiSingleStream dom) -> Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (BitVector 8))
withCircuit circuit input = fromAxi <$> axiOutput where
    (_, axiOutput) = toSignals circuit (fmap toAxi <$> input, C.pure $ ack True)
    fromAxi :: AxiSingleStreamFwd -> Maybe (BitVector 8)
    fromAxi axiInp = case axiInp of
               Just axi -> if head (_tkeep axi) then Just $ unpack $ pack $ _tdata axi else Nothing
               Nothing -> Nothing
    toAxi :: BitVector 8 -> Axi4StreamM2S ('Axi4StreamConfig 1 0 0) ()
    toAxi bv = Axi4StreamM2S { _tdata = singleton $ unpack bv
                            , _tkeep = singleton True
                            , _tstrb = singleton False
                            , _tlast = False
                            , _tuser = ()
                            , _tid = 0
                            , _tdest = 0
                            }

-- | Convenience function to denote an ACK for an Axi4Stream succinctly
ack :: Bool -> Axi4StreamS2M
ack b = Axi4StreamS2M { _tready = b }

mealyToCircuit :: C.HiddenClockResetEnable dom
    => NFDataX a
    => (a -> (Maybe (Axi4StreamM2S conf user), Axi4StreamS2M) -> (a, (Axi4StreamS2M, Maybe (Axi4StreamM2S  conf user))))
    -> a
    -> Circuit (Axi4Stream dom conf user) (Axi4Stream dom conf user)
mealyToCircuit machineAsFunction initialState = Circuit $ circuitFunction where
  circuitFunction = C.unbundle
                  . C.mealy machineAsFunction initialState
                  . C.bundle

preambleInserter :: C.HiddenClockResetEnable dom => Circuit (AxiSingleStream dom) (AxiSingleStream dom)
preambleInserter = mealyToCircuit machineAsFunction 0 where
  -- 0: Just passing messages through like normal
  -- 1: Sending SFD (Start of Frame Delimiter)
  -- 2-8: Sending normal preamble
  machineAsFunction :: Index 9 -> (AxiSingleStreamFwd, Axi4StreamS2M) -> (Index 9, (Axi4StreamS2M, AxiSingleStreamFwd))
  machineAsFunction 0 (inp, recvACK) = (newCounter, (recvACK, inp))
    where
      -- If this was the last byte of the frame or not part of a frame,
      -- then a preamble should be inserted whenever a new frame begins.
      newCounter = if maybe True _tlast inp then 8 else 0
  machineAsFunction n (_, recvACK) = (nextStep, (ack False, Just out))
            where
                nextStep = if _tready recvACK then n-1 else n
                out = Axi4StreamM2S { _tdata = singleton $ if n == 1 then 0xd5 else 0x55
                                    , _tkeep = singleton True
                                    , _tstrb = singleton False
                                    , _tlast = False
                                    , _tuser = ()
                                    , _tid = 0
                                    , _tdest = 0
                                    }


ifgEnforcer :: C.HiddenClockResetEnable dom => Circuit (AxiSingleStream dom) (AxiSingleStream dom)
ifgEnforcer = mealyToCircuit machineAsFunction 0 where
  machineAsFunction :: Index 13 -> (AxiSingleStreamFwd, Axi4StreamS2M) -> (Index 13, (Axi4StreamS2M, AxiSingleStreamFwd))
  machineAsFunction 0 (inp, recvACK) = (newCounter, (recvACK, inp))
    where
      -- If this was the last byte of the frame, then skip 12 cycles for the Inter Frame Gap
      newCounter = if maybe False _tlast inp then 12 else 0
  machineAsFunction n (_, _) = (n-1, (ack False, Nothing))


streamTestFramePerSecond :: C.HiddenClockResetEnable dom => Circuit () (AxiStream dom)
streamTestFramePerSecond = Circuit $ circuitFunction where
  circuitFunction ((), recvACK) = ((), out) where
    -- initialize bram
    brContent :: Vec 18 (Vec 4 Byte)
    brContent = unpack $ pack testFrame
    brRead = C.blockRam brContent brReadAddr brWrite
    brWrite = C.pure Nothing

    -- run the state machine (a mealy machine)
    initialState = 200
    (brReadAddr, out)
      = C.unbundle
      $ C.mealy machineAsFunction initialState
      $ C.bundle (brRead, recvACK)

  machineAsFunction :: Unsigned 32 -> (Vec 4 Byte, Axi4StreamS2M) -> (Unsigned 32, (Unsigned 32, AxiStreamFwd))
  machineAsFunction rAddr0 (brRead0, popped) =
    let
        -- adjust blockram read address
        rAddr1
          | rAddr0 > 50_000_000 = 0
          | _tready popped || rAddr0 >= 18 = rAddr0+1
          | otherwise = rAddr0
        -- return our new state and outputs
        otpDat = if rAddr0 < 18 then Just Axi4StreamM2S { _tdata = brRead0
                               , _tkeep = replicate d4 True
                               , _tstrb = replicate d4 False
                               , _tlast = rAddr0 == 17
                               , _tuser = ()
                               , _tid = 0
                               , _tdest = 0
                               } else Nothing
    in  (rAddr1, (rAddr1, otpDat))
