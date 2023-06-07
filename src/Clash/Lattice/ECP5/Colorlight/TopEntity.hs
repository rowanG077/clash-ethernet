{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS -fplugin=Protocols.Plugin #-}

module Clash.Lattice.ECP5.Colorlight.TopEntity ( topEntity ) where

import Data.Maybe ( isNothing )
import Data.Proxy ( Proxy (Proxy) )

import Clash.Annotations.TH
import Clash.Cores.Ethernet.MAC ( macCircuits )
import Clash.Cores.Ethernet.MAC.Packetizer
import Clash.Cores.Ethernet.RGMII
import Clash.Cores.Ethernet.StaticFrame
import Clash.Cores.Ethernet.Stream ( TaggedStream )

import Clash.Explicit.Prelude
import Clash.Lattice.ECP5.Colorlight.CRG
import Clash.Lattice.ECP5.Prims
import Clash.Signal ( exposeClockResetEnable, HiddenClockResetEnable, withClockResetEnable )

import Protocols
import Protocols.DfConv ( void, fanout )

import Clash.Cores.Ethernet.MDIO ( mdioComponent )
import Clash.Lattice.ECP5.Colorlight.Bridge ( uartToMdioBridge )
import Clash.Lattice.ECP5.Colorlight.SendCounter

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
topEntity clk25 uartRxBit _dq_in mdio_in eth0_rx eth1_rx =
  let
    (clk50, _clkEthTx, rst50, _rstEthTx) = crg clk25
    en50 = enableGen
    _enEthTx = enableGen

    -- MDIO component
    mdio, mdioReg :: Signal Dom50 Bit
    (mdio_output, mdio) = bb mdio_in
                         (register clk50 rst50 en50 1 $ boolToBit . isNothing <$> mdioWrite)
                         (ofs1p3bx clk50 rst50 en50 $ fromJustX <$> mdioWrite)

    mdioReg = ifs1p3bx clk50 rst50 en50 mdio
    (mdioWrite, mdioResponse, mdc) = (exposeClockResetEnable mdioComponent clk50 rst50 en50) mdioReg mdioRequest

    -- UART-MDIO bridge
    (uartTxBit, mdioRequest) = (ofs1p3bx clk50 rst50 en50 $ txBit, req)
      where
        (txBit, req) = (exposeClockResetEnable uartToMdioBridge clk50 rst50 en50) (SNat @9600) rxBit mdioResponse
        rxBit = ifs1p3bx clk50 rst50 en50 uartRxBit

    -- TODO: What to do with this one?
    dq_out = undefined

    {- ETH0 ~ RGMII SETUP -}
    eth0Txclk = rgmii_rx_clk eth0_rx

    {- ETH1 ~ RGMII SETUP -}
    eth1Txclk = rgmii_rx_clk eth1_rx

    {- SETUP MAC LAYER -}
    ((), eth0Tx) = toSignals circ (eth0_rx,())
      where
        with50 = withClockResetEnable clk50 rst50 en50

        mainLogic = with50 $ circuit $ \inp -> do
          [inp1, inp2] <- (fanout Proxy Proxy :: HiddenClockResetEnable dom => Circuit (TaggedStream dom) (Vec 2 (TaggedStream dom))) -< inp

          out1 <- sendCounterPerSecond <| void Proxy -< inp1
          out2 <- streamTestFramePerSecond <| (void Proxy :: HiddenClockResetEnable dom => Circuit (TaggedStream dom) ()) -< inp2

          macPacketizer -< [out1, out2]

        circ = do
          let (rxMAC, txMAC) = macCircuits eth0Txclk resetGen enableGen clk50 rst50 en50
          let (rxRGMII, txRGMII) = rgmiiCircuits eth0Txclk resetGen enableGen (delayg d80) (delayg d0) (\a b _ -> iddrx1f a b) (\a b _ -> oddrx1f a b)

          txRGMII <| txMAC <| mainLogic <| rxMAC <| rxRGMII

    ((), eth1Tx) = toSignals circ (eth1_rx,())
      where
        with50 = withClockResetEnable clk50 rst50 en50

        mainLogic = with50 $ do
            streamTestFramePerSecond <| void Proxy

        circ = do
          let (rxMAC, txMAC) = macCircuits eth1Txclk resetGen enableGen clk50 rst50 en50
          let (rxRGMII, txRGMII) = rgmiiCircuits eth1Txclk resetGen enableGen (delayg d80) (delayg d0) (\a b _ -> iddrx1f a b) (\a b _ -> oddrx1f a b)

          txRGMII <| txMAC <| mainLogic <| rxMAC <| rxRGMII

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
          { mdio_out = mdio_output
          , mdio_mdc = mdc
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
