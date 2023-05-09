{-# LANGUAGE NumericUnderscores #-}

module Clash.Cores.Ethernet.Stream (streamTestFramePerSecond, ifgEnforcer, preambleInserter, fcsAppender, mealyToCircuit, AxiStream, AxiSingleStream) where

import Data.Maybe (isNothing)

import qualified Clash.Prelude as C
import Clash.Prelude
import Protocols
import Protocols.Df
import Protocols.Axi4.Stream
import Clash.Cores.Ethernet.Frame (testFrame)
import Clash.Cores.Ethernet.CRC ( CRCState, finish_crc_state, crc_starting_state, upd_crc_state )

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

-- | Convenience function to denote an ACK for an Axi4Stream succinctly
ack :: Bool -> Axi4StreamS2M
ack b = Axi4StreamS2M { _tready = b }

mealyToCircuit :: C.HiddenClockResetEnable dom
    => NFDataX a
    => (a -> (Maybe (Axi4StreamM2S conf1 user), Axi4StreamS2M) -> (a, (Axi4StreamS2M, Maybe (Axi4StreamM2S conf2 user))))
    -> a
    -> Circuit (Axi4Stream dom conf1 user) (Axi4Stream dom conf2 user)
mealyToCircuit machineAsFunction initialState = Circuit $ circuitFunction where
  circuitFunction = C.unbundle
                  . C.mealy machineAsFunction initialState
                  . C.bundle

-- | Inserts the preamble in front of a packet.
--
-- From the moment this circuit starts `Just` receiving data it starts outputting the preamble.
-- It thus delays the stream by exactly 8 cycles.
preambleInserter :: C.HiddenClockResetEnable dom => Circuit (AxiSingleStream dom) (AxiSingleStream dom)
preambleInserter = mealyToCircuit machineAsFunction 8 where
  -- 0: Just passing messages through like normal
  -- 1: Sending SFD (Start of Frame Delimiter)
  -- 2-7: Sending normal preamble
  -- 8: Waiting for beginning of message, if found then start sending preamble
  machineAsFunction :: Index 9 -> (AxiSingleStreamFwd, Axi4StreamS2M) -> (Index 9, (Axi4StreamS2M, AxiSingleStreamFwd))
  machineAsFunction 0 (inp, recvACK) = (newCounter, (recvACK, inp))
    where
      -- If this was the last byte of the frame or not part of a frame,
      -- then a preamble should be inserted whenever a new frame begins.
      newCounter = if maybe False _tlast inp then 8 else 0
  machineAsFunction n (inp, recvACK) = (nextStep, (ack False, out))
            where
                nextStep
                  | isNothing inp = n
                  | _tready recvACK = n-1
                  | otherwise = n
                out
                  | isNothing inp = Nothing
                  | otherwise = Just Axi4StreamM2S { _tdata = singleton $ if n == 1 then 0xd5 else 0x55
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


data FCSState = Accumulating CRCState | Appending (Index 4) (Vec 4 Byte)
  deriving (Show, Eq, Generic, NFDataX)

-- | Calculates and appends the FCS (checksum) to the frame
fcsAppender :: C.HiddenClockResetEnable dom => Circuit (AxiSingleStream dom) (AxiSingleStream dom)
fcsAppender = mealyToCircuit machineAsFunction starting_state where
  starting_state = Accumulating crc_starting_state
  reverseEndianness :: forall n . KnownNat n => BitVector n -> BitVector n
  reverseEndianness bv = pack $ reverse vec
      where
        vec :: Vec n Bit
        vec = unpack bv

  machineAsFunction :: FCSState -> (AxiSingleStreamFwd, Axi4StreamS2M) -> (FCSState, (Axi4StreamS2M, AxiSingleStreamFwd))
  machineAsFunction (Accumulating s) (Just inp, recvACK) = (newState, (recvACK, out))
    where
      -- Always set _tlast to False
      out = Just Axi4StreamM2S { _tdata = _tdata inp
                               , _tkeep = _tkeep inp
                               , _tstrb = _tstrb inp
                               , _tlast = False
                               , _tuser = _tuser inp
                               , _tid = _tid inp
                               , _tdest = _tdest inp
                               }
      -- Update the CRC state with an extra byte of input
      newCRC = upd_crc_state s (reverseEndianness $ pack $ _tdata inp)
      newState
        -- No transmit taking place this cycle, keep in same state
        | not (_tready recvACK) = Accumulating s
        -- No transmit taking place this cycle, keep in same state
        | _tlast inp = Appending 0 (unpack $ finish_crc_state newCRC)
        | otherwise = Accumulating newCRC
  machineAsFunction (Appending ind bv) (_, recvACK) = (newState, (ack False, out))
    where
      -- NOTE: Take care with ordering!
      out = Just Axi4StreamM2S { _tdata = singleton $ unpack $ reverseEndianness $ pack $ bv !! ind
                               , _tkeep = singleton True
                               , _tstrb = singleton False
                               , _tlast = ind == 3
                               , _tuser = ()
                               , _tid = 0
                               , _tdest = 0
                               }
      newState | not $ _tready recvACK = Appending ind bv
               | ind == 3 = starting_state
               | otherwise = Appending (ind+1) bv
  machineAsFunction s (Nothing, recvACK) = (s, (recvACK, Nothing))

streamTestFramePerSecond :: C.HiddenClockResetEnable dom => Circuit () (AxiStream dom)
streamTestFramePerSecond = Circuit $ circuitFunction where
  circuitFunction ((), recvACK) = ((), out) where
    -- initialize bram
    brContent :: Vec 15 (Vec 4 Byte)
    brContent = takeI $ unpack $ pack $ drop d8 testFrame
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
          | _tready popped || rAddr0 >= 15 = rAddr0+1
          | otherwise = rAddr0
        -- return our new state and outputs
        otpDat = if rAddr0 < 15 then Just Axi4StreamM2S { _tdata = brRead0
                               , _tkeep = replicate d4 True
                               , _tstrb = replicate d4 False
                               , _tlast = rAddr0 == 14
                               , _tuser = ()
                               , _tid = 0
                               , _tdest = 0
                               } else Nothing
    in  (rAddr1, (rAddr1, otpDat))
