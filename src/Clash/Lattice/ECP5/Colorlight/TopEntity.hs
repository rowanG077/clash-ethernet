{-# LANGUAGE NumericUnderscores #-}

module Clash.Lattice.ECP5.Colorlight.TopEntity ( topEntity ) where

import Clash.Annotations.TH
import Clash.Cores.Ethernet.Frame ( sendFrameOnPulse )
import Clash.Cores.Ethernet.RGMII
    ( RGMIIRXChannel(..), RGMIITXChannel(..), rgmiiReceiver, rgmiiSender )
import Clash.Explicit.Prelude
import Clash.Lattice.ECP5.Colorlight.CRG
import Clash.Lattice.ECP5.Prims
import Clash.Signal ( exposeClockResetEnable, hideClockResetEnable )

import qualified Clash.Prelude as I

import Data.Maybe (isNothing)

import Clash.Cores.UART

data SDRAMOut domain = SDRAMOut
  {
    sdram_clock :: "clk" :::Clock domain,
    sdram_a :: "a" ::: Signal domain (BitVector 11),
    sdram_we_n :: "we_n" ::: Signal domain Bit,
    sdram_ras_n :: "ras_n" :::Signal domain Bit,
    sdram_cas_n :: "cas_n" ::: Signal domain Bit,
    sdram_ba :: "ba" ::: Signal domain (BitVector 2),
    sdram_dq :: "dq" ::: BiSignalOut 'Floating domain 32
  }

data MDIOOut domain = MDIOOut
  {
    mdio_out :: "mdio" ::: BiSignalOut 'Floating domain 1,
    mdio_mdc :: "mdc" ::: Signal domain Bit
  }

data HubOut domain = HubOut
  {
    hub_clk :: "clk" ::: Signal domain Bit,
    hub_line_select :: "line_select" ::: Signal domain (BitVector 5),
    hub_latch :: "latch" ::: Signal domain Bit,
    hub_output_enable :: "output_enable" ::: Signal domain Bit,
    hub_data :: "data" ::: Signal domain (BitVector 48)
  }

topEntity
  :: "clk25" ::: Clock Dom25
  -> "uart_rx" ::: Signal Dom50 Bit
  -> "sdram_dq" ::: BiSignalIn 'Floating Dom50 32
  -> "eth_mdio" ::: BiSignalIn 'Floating Dom50 1
  -> "eth0" ::: RGMIIRXChannel DomEth0 DomDDREth0
  -> "eth1" ::: RGMIIRXChannel DomEth1 DomDDREth1
  -> ( "uart_tx" ::: Signal Dom50 Bit
     , "sdram" ::: SDRAMOut Dom50
     , "eth" ::: MDIOOut Dom50
     , "eth0" ::: RGMIITXChannel DomDDREth0
     , "eth1" ::: RGMIITXChannel DomDDREth1
     , "hub" ::: HubOut Dom50
     )

topEntity clk25 uartRxBit dq_in mdio_in eth0_rx eth1_rx =
  let
    (clk50, rst50) = crg clk25
    en50 = enableGen

    -- Simply echo back uart signals through IO flip flops
    uartTxBit = ofs1p3bx clk50 rst50 en50 $ ifs1p3bx clk50 rst50 en50 uartRxBit

    {- MDIO SETUP -}
    dq :: Signal Dom50 (BitVector 32)
    mdio :: Signal Dom50 Bit
    (dq_out, dq) = bb dq_in onoff (ofs1p3bx clk50 rst50 en50 dqReg) -- sdram_dq
    (eth_mdio_out, mdio) = bb mdio_in onoff (ofs1p3bx clk50 rst50 en50 mdioReg) -- sdram_dq

    dqReg = ifs1p3bx clk50 rst50 en50 dq
    mdioReg = ifs1p3bx clk50 rst50 en50 mdio

    onoff = register clk50 rst50 en50 0 $ fmap complement onoff

    {- ETH0 ~ RGMII SETUP -}
    eth0Txclk = rgmii_rx_clk eth0_rx
    macInput = rgmiiReceiver eth0_rx (SNat @80)
    eth0Tx = rgmiiSender eth0Txclk resetGen enableGen (SNat @0) macOutput

    {- ETH1 ~ RGMII SETUP -}
    eth1Txclk = rgmii_rx_clk eth1_rx
    macInput1 = rgmiiReceiver eth1_rx (SNat @80)
    eth1Tx = rgmiiSender eth1Txclk resetGen enableGen (SNat @0) macOutput1

    {- SETUP MAC LAYER -}
    -- macOutput = macInput
    -- macOutput1 = macInput1
    -- macOutput = exposeClockResetEnable srcAddressChanger eth0Txclk resetGen enableGen $ macInput
    -- macOutput1 = exposeClockResetEnable srcAddressChanger eth1Txclk resetGen enableGen $ macInput1
    macOutput = exposeClockResetEnable (sendFrameOnPulse (hideClockResetEnable riseEvery (SNat :: SNat 1_000)) $ macInput) eth0Txclk resetGen enableGen
    macOutput1 = exposeClockResetEnable (sendFrameOnPulse (hideClockResetEnable riseEvery (SNat :: SNat 1_000)) $ macInput1) eth1Txclk resetGen enableGen

    srcAddressChanger :: forall dom . I.HiddenClockResetEnable dom => Signal dom (Maybe (BitVector 8)) -> Signal dom (Maybe (BitVector 8))
    srcAddressChanger inp = fmap change_byte $ bundle (inp, counter)
            where
                change_byte :: (Maybe (BitVector 8), Unsigned 16) -> (Maybe (BitVector 8))
                change_byte (i,18) = fmap (xor 0b1000_0000) i
                change_byte (i,19) = fmap (xor 0b1) i
                change_byte (i,_)  = i

                counter :: Signal dom (Unsigned 16)
                counter = (I.register) 0 (inc <$> bundle (counter, inp))
                    where
                        inc (i,p)
                            | isNothing p = 0
                            | otherwise = i+1

    in
      ( uartTxBit
      , SDRAMOut
          { sdram_clock = clk50
          , sdram_a = pure 0
          , sdram_we_n = pure 1
          , sdram_ras_n = pure 1
          , sdram_cas_n = pure 1
          , sdram_ba = pure 0
          , sdram_dq = dq_out
          }
      , MDIOOut
          { mdio_out = eth_mdio_out
          , mdio_mdc = pure 0
          }
      , eth0Tx
      , eth1Tx
      , HubOut
          { hub_clk = pure 0
          , hub_line_select = pure 0
          , hub_latch = pure 0
          , hub_output_enable = pure 0
          , hub_data = pure 0
          }
      )

makeTopEntity 'topEntity
