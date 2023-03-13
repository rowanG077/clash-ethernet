module Clash.Cores.Ethernet.RGMII (rgmii) where

import Clash.Prelude

-- NOTE: for now transmission error is not considered
rgmiiSender :: 
  Maybe (BitVector 8)
  -- ^ data to send
  -> (Bit, Bit, BitVector 4, Bitvector 4)
  -- ^ tx_err, tx_en, data1, data2
rgmiiSender Nothing
  = (0, 0, 0, 0) -- set tx_en low
rgmiiSender (Just bv)
  = (0, 1, d1, d2) -- set tx_en hight
  where
    (d1, d2) = split bv


rgmii :: HiddenClock domTX
  => HiddenClockResetEnable domRX
  => KnownDomain domTX
  -- ^ tx domain
  => KnownDomain domRX
  -- ^ rx domain
  => Signal domTX (BitVector 8)
  -- ^ data to send
  -> Signal domRX (BitVector 4, Bit)
  -- ^ rx_data and rx_ctl
  -> Signal domRX (Vec 6 (Bit, Bit, Bit))
  -- ^ delay signals (LOADN, MOVE, DIRECTION)
  -> (Signal domRX (BitVector 4, Bit), 
      Signal domTX (BitVector 8))
  -- ^ ((tx_data, tx_ctl), received data)

rgmii = error "Not Implemented"