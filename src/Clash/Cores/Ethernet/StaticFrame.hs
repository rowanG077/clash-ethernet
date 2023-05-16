{-# language NumericUnderscores #-}

module Clash.Cores.Ethernet.StaticFrame ( streamTestFramePerSecond ) where

import Clash.Prelude
import qualified Clash.Prelude as C
import Protocols
import Protocols.Axi4.Stream

import Clash.Cores.Ethernet.CRC ( CRCState, crc_starting_state, finish_crc_state, upd_crc_state )
import Clash.Cores.Ethernet.Frame ( testFrame )
import Clash.Cores.Ethernet.Stream ( EthernetHeader, TaggedStream, TaggedStreamFwd, mealyToCircuit )

type Byte = Unsigned 8

-- | Continually streams the given header+payload every 50 million clock cycles
streamTestFramePerSecond :: forall n dom
  . C.HiddenClockResetEnable dom
  => Unsigned 32
  -> Vec n (Vec 4 Byte)
  -> EthernetHeader
  -> Circuit () (TaggedStream dom)
streamTestFramePerSecond len brContent header = Circuit $ circuitFunction where
  circuitFunction ((), recvACK) = ((), out) where
    -- initialize bram
    brRead = C.blockRam brContent brReadAddr brWrite
    brWrite = C.pure Nothing

    -- Do not start sending a packet immediately as it gets garbled in the REPL for the first few clock cycles
    -- Eases debugging
    initialState = len+1
    -- run the state machine (a mealy machine)
    (brReadAddr, out)
      = C.unbundle
      $ C.mealy machineAsFunction initialState
      $ C.bundle (brRead, recvACK)

  machineAsFunction :: Unsigned 32 -> (Vec 4 Byte, Axi4StreamS2M) -> (Unsigned 32, (Unsigned 32, TaggedStreamFwd))
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
                               , _tuser = header
                               , _tid = 0
                               , _tdest = 0
                               } else Nothing
    in  (rAddr1, (rAddr1, otpDat))
