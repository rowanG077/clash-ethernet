module Clash.Cores.Ethernet.MAC.Preamble ( preambleInserter ) where

import Clash.Prelude
import Data.Maybe ( isNothing )
import Protocols
import Protocols.Axi4.Stream

import Clash.Cores.Ethernet.Stream ( SingleByteStream, SingleByteStreamFwd, mealyToCircuit )

-- | Inserts the preamble in front of a packet.
--
-- From the moment this circuit starts `Just` receiving data it starts outputting the preamble.
-- It thus delays the stream by exactly 8 cycles.
preambleInserter :: HiddenClockResetEnable dom => Circuit (SingleByteStream dom) (SingleByteStream dom)
preambleInserter = mealyToCircuit machineAsFunction 8 where
  -- 0: Just passing messages through like normal
  -- 1: Sending SFD (Start of Frame Delimiter)
  -- 2-7: Sending normal preamble
  -- 8: Waiting for beginning of message, if found then start sending preamble
  machineAsFunction :: Index 9 -> (SingleByteStreamFwd, Axi4StreamS2M) -> (Index 9, (Axi4StreamS2M, SingleByteStreamFwd))
  machineAsFunction 0 (inp, recvACK) = (newCounter, (recvACK, inp))
    where
      -- If this was the last byte of the frame or not part of a frame,
      -- then a preamble should be inserted whenever a new frame begins.
      newCounter = if maybe False _tlast inp then 8 else 0
  machineAsFunction n (inp, recvACK) = (nextStep, (notReady, out))
            where
                notReady = Axi4StreamS2M { _tready = False }
                nextStep
                  | isNothing inp = n
                  | _tready recvACK = n-1
                  | otherwise = n
                out
                  | isNothing inp = Nothing
                  | otherwise = Just Axi4StreamM2S
                                  { _tdata = singleton $ if n == 1 then 0xd5 else 0x55
                                  , _tkeep = singleton True
                                  , _tstrb = singleton False
                                  , _tlast = False
                                  , _tuser = ()
                                  , _tid = 0
                                  , _tdest = 0
                                  }
