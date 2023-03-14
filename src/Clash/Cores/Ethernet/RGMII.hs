module Clash.Cores.Ethernet.RGMII ( rgmiiSender, rgmiiReceiver ) where

import Clash.Prelude
import Clash.Lattice.ECP5.Prims
import Data.Maybe ( isJust )

-- NOTE: for now transmission error is not considered
rgmiiSender :: HiddenClockResetEnable dom
  => Signal dom (Maybe (BitVector 8))
  -- ^ data to send
  -> Signal dom (Bit, Bit, BitVector 4, BitVector 4)
  -- ^ tx_err, tx_en, data1, data2
rgmiiSender input = bundle (txErr, txEn, d1, d2) -- set tx_en high
  where
    txEnBool = isJust <$> input
    txErr = pure 0
    txEn = boolToBit <$> txEnBool
    (d1, d2) = unbundle $ maybe (0, 0) split <$> input

rgmiiReceiver :: HiddenClockResetEnable dom
  => Signal dom Bit 
  -- ^ tx_err
  -> Signal dom Bit 
  -- ^ tx_val
  -> Signal dom  (BitVector 4)
  -- ^ 4 msb of transmission (to be joined)
  -> Signal dom (BitVector 4)
  -- ^ 4 lsb of transmission (to be joined)
  -> Signal dom (BitVector 8)
rgmiiReceiver txErr rxVal bv1 bv2 = (++#) <$> bv1 <*> bv2