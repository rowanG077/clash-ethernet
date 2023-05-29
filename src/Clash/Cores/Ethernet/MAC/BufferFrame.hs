{-# OPTIONS -fplugin=Protocols.Plugin #-}
{-# language FlexibleContexts #-}

module Clash.Cores.Ethernet.MAC.BufferFrame ( bufferFrame ) where

import Clash.Prelude
import qualified Clash.Prelude as C
import Data.Maybe ( isJust )
import Data.Proxy ( Proxy(Proxy) )
import Protocols
import Protocols.Axi4.Stream
import qualified Protocols.DfConv as Df

import Clash.Cores.Ethernet.Stream ( SingleByteStream, SingleByteStreamFwd, mealyToCircuit )

data State = State (Index 1500)
  deriving (Show, Eq, Generic, NFDataX)

type Byte = BitVector 8

-- | A fifo buffer making sure there are no "holes" in the Ethernet frame by
-- waiting until it is completely buffered.
-- Can handle simultaneous write and read (full throughput rate).
--
-- Uses the fifo component of clash-protocols.
bufferFrame :: forall dom conf . (KnownAxi4StreamConfig conf) => C.HiddenClockResetEnable dom => Circuit (Axi4Stream dom conf ()) (Axi4Stream dom conf ())
bufferFrame = circuit $ \inp -> do
  [inp1, inp2] <- Df.fanout Proxy Proxy -< inp

  -- Store input in a fifo
  out <- buf -< inp1

  -- Output only frames that have been completely buffered
  restrictOutput -< (inp2, out)
    where
      buf :: C.HiddenClockResetEnable dom => Circuit (Axi4Stream dom conf ()) (Axi4Stream dom conf ())
      buf = Df.fifo Proxy Proxy (SNat @1500)

      -- NOTE: Rewrite this using mealy machine? I'm sorry it's a bit of a mess.
      restrictOutput :: C.HiddenClockResetEnable dom => Circuit (Axi4Stream dom conf (), Axi4Stream dom conf ()) (Axi4Stream dom conf ())
      restrictOutput = Circuit circuitFunction
        where
          ready = pure Axi4StreamS2M { _tready = True }

          circuitFunction ((bufIn, bufOut), recvACK) = ((ready, ackBuf <$> bundle (recvACK, out)), out)
           where
             -- Only allow output from buffer if we are successfully outputting it ourselves.
             ackBuf (r,o) = Axi4StreamS2M { _tready = _tready r && isJust o }

             -- Only output when at least 1 frame has been fully buffered
             out = (\(ind,o) -> if ind > 0 then o else Nothing) <$> bundle (cnt, bufOut)

             cnt :: Signal dom (Index 1500)
             cnt = register 0 $ upd <$> bundle (isLast <$> bundle (bufIn, ready), isLast <$> bundle (bufOut, recvACK), cnt)
               where
                 -- Is the last byte of a frame being transferred?
                 isLast (p, ack) = _tready ack && maybe False _tlast p

                 -- Update the counter of how many frames can be safely outputted.
                 upd (i, o, ind)
                   | o && i = ind
                   | i = ind+1
                   | o = ind-1
                   | otherwise = ind
