module Clash.Cores.Ethernet.RGMII ( rgmiiSender, rgmiiReceiver, RGMIIRXChannel (..), RGMIITXChannel (..) ) where

import Clash.Lattice.ECP5.Colorlight.CRG
import Clash.Lattice.ECP5.Prims
import Clash.Prelude
import Data.Maybe ( isJust )


-- NOTE: make ddrDomain generic -> 2 * ddrDomain frequency = domain frequency
data RGMIIRXChannel domain ddrDomain = RGMIIRXChannel
  {
    rgmii_rx_clk :: "rx_clk" ::: Clock domain,
    rgmii_rx_ctl :: "rx_ctl" ::: Signal ddrDomain Bit,
    rgmii_rx_data :: "rx_data" ::: Signal ddrDomain (BitVector 4)
  }

data RGMIITXChannel ddrDomain = RGMIITXChannel
  {
    rgmii_tx_clk :: "tx_clk" ::: Signal ddrDomain Bit,
    rgmii_tx_ctl :: "tx_ctl" ::: Signal ddrDomain Bit,
    rgmii_tx_data :: "tx_data" ::: Signal ddrDomain (BitVector 4)
  }

-- | sender component of RGMII -> NOTE: for now transmission error is not considered
rgmiiSender :: forall dom domDDR delay fPeriod edge reset init polarity . delay <= 127
  => KnownConfiguration domDDR ('DomainConfiguration domDDR fPeriod edge reset init polarity)     -- 0
  => KnownConfiguration dom ('DomainConfiguration dom (2*fPeriod) edge reset init polarity) -- 1
  => Clock dom -- the DDR tx clock
  -> Reset dom
  -> Enable dom
  -> SNat delay
  -- ^ tx delay needed
  -> Signal dom (Maybe (BitVector 8))
  -- ^ Maybe the byte we have to output
  -> RGMIITXChannel domDDR
  -- ^ tx channel to the phy
rgmiiSender txClk rst en delay input = RGMIITXChannel
  { rgmii_tx_clk = txdelay $ oddrx1f txClk rst (pure 1) (pure 0) -- clock forwarding
  , rgmii_tx_ctl = txdelay txCtl
  , rgmii_tx_data = txdelay txData
  }
  where
    txEn, txErr :: Signal dom Bit
    txEn = boolToBit . isJust <$> input -- set tx_en high
    txErr = pure 0 -- for now error always low
    data1, data2 :: Signal dom (BitVector 4)
    (data1, data2) = unbundle $ maybe (0, 0) split <$> input

    -- multiplex signals
    txCtl :: Signal domDDR Bit
    txCtl = oddrx1f txClk rst txErr txEn
    txData :: Signal domDDR (BitVector 4)
    txData = oddrx1f txClk rst data1 data2

    -- define txdelay
    txdelay :: Signal domDDR a -> Signal domDDR a
    txdelay = delayg delay


-- | receiver component of RGMII
rgmiiReceiver :: forall (dom :: Domain) (domDDR :: Domain) delay fPeriod edge reset init polarity .
  KnownConfiguration domDDR ('DomainConfiguration domDDR fPeriod edge reset init polarity) -- 0
  => KnownConfiguration dom ('DomainConfiguration dom (2*fPeriod) edge reset init polarity) -- 1
  => delay <= 127
  => RGMIIRXChannel dom domDDR
  -- ^ rx channel from phy
  -> SNat delay
  -- ^ delays
  -> Signal dom (Maybe (BitVector 8))
  -- ^ received data
rgmiiReceiver channel delay = macInput
  where
    -- define rxdelay
    rxdelay :: Signal domDDR a -> Signal domDDR a
    rxdelay = delayg delay

    -- extract channel
    ethRxClk :: Clock dom
    ethRxClk = rgmii_rx_clk channel
    _ethRxCtl :: Signal domDDR Bit
    _ethRxCtl = rgmii_rx_ctl channel
    _ethRxData :: Signal domDDR (BitVector 4)
    _ethRxData = rgmii_rx_data channel

    -- delay input signals
    ethRxCtl :: Signal domDDR Bit
    ethRxCtl = rxdelay _ethRxCtl
    ethRxData :: Signal domDDR (BitVector 4)
    ethRxData = rxdelay _ethRxData

    -- demultiplex signal
    ethRxErr, ethRxDv :: Signal dom Bit
    (ethRxErr, ethRxDv) = unbundle $ iddrx1f ethRxClk resetGen ethRxCtl
    ethRxData1, ethRxData2 :: Signal dom (BitVector 4)
    (ethRxData1, ethRxData2) = unbundle $ iddrx1f ethRxClk resetGen ethRxData

    -- rgmii component -> send data to mac when rxDv is high
    macInput :: Signal dom (Maybe (BitVector 8))
    macInput = mux (bitToBool <$> ethRxDv) (fmap Just $ (++#) <$> ethRxData1 <*> ethRxData2) (pure Nothing)
