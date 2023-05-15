{-# language NumericUnderscores #-}

module Clash.Cores.Ethernet.Stream
( streamTestFramePerSecond
, mealyToCircuit
, SingleByteStream
, SingleByteStreamFwd
, FourByteStream
, FourByteStreamFwd
, TaggedStream
) where


import Clash.Cores.Ethernet.Frame ( testFrame )
import Clash.Prelude
import qualified Clash.Prelude as C
import Protocols
import Protocols.Axi4.Stream
import Protocols.Df


type Byte = Unsigned 8

-- | An untagged 4-byte wide stream
type FourByteStream (dom :: Domain) = Axi4Stream dom ('Axi4StreamConfig 4 0 0) ()
type FourByteStreamFwd = Maybe (Axi4StreamM2S ('Axi4StreamConfig 4 0 0) ())

-- | An untagged 1-byte wide stream
type SingleByteStream (dom :: Domain) = Axi4Stream dom ('Axi4StreamConfig 1 0 0) ()
type SingleByteStreamFwd = Maybe (Axi4StreamM2S ('Axi4StreamConfig 1 0 0) ())

-- | A tagged 4-byte wide stream, used for routing
type EthernetHeader = (Vec 6 Byte, Vec 6 Byte, BitVector 16)
type TaggedStream (dom :: Domain) = Axi4Stream dom ('Axi4StreamConfig 4 0 0) EthernetHeader


mealyToCircuit :: C.HiddenClockResetEnable dom
    => NFDataX a
    => (a -> (Maybe (Axi4StreamM2S conf1 user), Axi4StreamS2M) -> (a, (Axi4StreamS2M, Maybe (Axi4StreamM2S conf2 user))))
    -> a
    -> Circuit (Axi4Stream dom conf1 user) (Axi4Stream dom conf2 user)
mealyToCircuit machineAsFunction initialState = Circuit $ circuitFunction where
  circuitFunction = C.unbundle
                  . C.mealy machineAsFunction initialState
                  . C.bundle


-- | Continually streams the given header+payload every 50 million clock cycles
streamTestFramePerSecond :: forall n dom . C.HiddenClockResetEnable dom => Unsigned 32 -> Vec n (Vec 4 Byte) -> Circuit () (FourByteStream dom)
streamTestFramePerSecond len brContent = Circuit $ circuitFunction where
  circuitFunction ((), recvACK) = ((), out) where
    -- initialize bram
    brRead = C.blockRam brContent brReadAddr brWrite
    brWrite = C.pure Nothing

    -- run the state machine (a mealy machine)
    initialState = len+1
    (brReadAddr, out)
      = C.unbundle
      $ C.mealy machineAsFunction initialState
      $ C.bundle (brRead, recvACK)

  machineAsFunction :: Unsigned 32 -> (Vec 4 Byte, Axi4StreamS2M) -> (Unsigned 32, (Unsigned 32, FourByteStreamFwd))
  machineAsFunction rAddr0 (brRead0, popped) =
    let
        -- adjust blockram read address
        rAddr1
          | rAddr0 > 50_000_000 = 0
          | _tready popped || rAddr0 >= len = rAddr0+1
          | otherwise = rAddr0
        -- return our new state and outputs
        otpDat = if rAddr0 < len then Just Axi4StreamM2S { _tdata = brRead0
                               , _tkeep = replicate d4 True
                               , _tstrb = replicate d4 False
                               , _tlast = rAddr0 == len-1
                               , _tuser = ()
                               , _tid = 0
                               , _tdest = 0
                               } else Nothing
    in  (rAddr1, (rAddr1, otpDat))
