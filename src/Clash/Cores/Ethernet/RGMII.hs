module Clash.Cores.Ethernet.RGMII (rgmii) where

import Clash.Prelude
import Clash.Lattice.ECP5.Prims

-- NOTE: for now transmission error is not considered
rgmiiTX :: 
  Maybe (BitVector 8)
  -- ^ data to send
  -> (Bit, Bit, BitVector 4, Bitvector 4)
  -- ^ tx_err, tx_en, data1, data2
rgmiiSender input () ()
  = bundle (txErr, txEn, d1, d2) -- set tx_en high
  where
    txEnBool = isJust <$> input
    txErr = pure 0
    txEn = boolToBit <$> txEnBool
    (d1, d2) = split bv


rgmiiRX :: HiddenClockResetEnable dom
  => Signal dom Bit 
  -- ^ tx_err
  -> Signal dom Bit 
  -- ^ tx_val
  -> Signal dom  (BitVector 4)
  -- ^ 4 msb of transmission (to be joined)
  -> Signal dom (BitVector 4)
  -- ^ 4 lsb of transmission (to be joined)
  -> Signal dom (BitVector 8)
rgmiiRX txErr rxVal bv1 bv2 = ++# <$> bv1 <*> bv2

rgmiiSender