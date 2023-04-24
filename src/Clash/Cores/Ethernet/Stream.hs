{-# LANGUAGE NumericUnderscores #-}

module Clash.Cores.Ethernet.Stream (streamTestFramePerSecond) where

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
