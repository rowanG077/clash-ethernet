{-# language NumericUnderscores #-}

module Clash.Lattice.ECP5.Colorlight.SendCounter ( sendCounterPerSecond ) where

import Clash.Cores.Ethernet.Stream
import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream

data State = State (Unsigned 32) (Unsigned 32)
  deriving (Show, Eq, Generic, NFDataX)

-- | Continually streams an increasing counter every 50 million clock cycles
sendCounterPerSecond :: HiddenClockResetEnable dom => Circuit () (TaggedStream dom)
sendCounterPerSecond = Circuit $ circuitFunction where
  header = EthernetHeader
               { destinationMAC = unpack (0xffffffffffff :: BitVector (6*8))
               , sourceMAC = unpack (0xabcdef012345 :: BitVector (6*8))
               , etherType = 0x07ff
               }

  -- Do not start sending a packet immediately as it gets garbled in the REPL for the first few clock cycles
  -- Eases debugging
  initialState = State 1 0

  circuitFunction ((), recvACK) = ((), out)
    where
      out = unbundle
          $ mealy machineAsFunction initialState
          $ bundle recvACK

  machineAsFunction :: State -> Axi4StreamS2M -> (State, TaggedStreamFwd)
  machineAsFunction (State timer cnt) recvACK = (newState, out)
    where
      newState
        -- | timer > 1_000 = State 0 cnt
        | timer > 50_000_000 = State 0 cnt
        | timer > 0 = State (timer+1) cnt
        | _tready recvACK = State (timer+1) (satSucc SatWrap cnt)
        | otherwise = State timer cnt
      -- Only output when the timer is 0
      out
        | timer == 0 = Just Axi4StreamM2S { _tdata = unpack $ pack cnt
                                          , _tkeep = replicate d4 True
                                          , _tstrb = replicate d4 True
                                          , _tlast = True
                                          , _tuser = header
                                          , _tid = 0
                                          , _tdest = 0
                                          }
        | otherwise = Nothing
