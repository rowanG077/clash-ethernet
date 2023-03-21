module Clash.Lattice.ECP5.Colorlight.TopEntity (topEntity) where

import Clash.Annotations.TH
import Clash.Explicit.Prelude
import Clash.Cores.Ethernet.RGMII (rgmiiReceiver, rgmiiSender)
import Clash.Lattice.ECP5.Colorlight.CRG
import Clash.Lattice.ECP5.Prims

import Clash.Cores.UART

data RGMIIChannel domain = RGMIIChannel
  {
    rgmii_clk  :: "clk" ::: Clock domain,
    rgmii_ctl  :: "ctl" ::: Signal domain Bit,
    rgmii_data :: "data" ::: Signal domain (BitVector 4)
  }

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
  -> "eth0_rx" ::: RGMIIChannel DomEth0
  -> "eth1_rx" ::: RGMIIChannel DomEth1
  -> ( "uart_tx" ::: Signal Dom50 Bit
     , "sdram" ::: SDRAMOut Dom50
     , "eth" ::: MDIOOut Dom50
     , "eth0_tx" ::: RGMIIChannel DomEth0
     , "eth1_tx" ::: RGMIIChannel DomEth1
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
    -- generate tx_clk (125mHz)
    eth0TxClk = eth0RxClk --TODO change

    -- delay input signals
    eth0RxClk' = delayf _ _ _ eth0RxClk
    eth0RxCtl' = delayf _ _ _ _eth0RxCtl
    eth0RxData' = delayf _ _ _ _eth0RxData

    -- demultiplex signal
    (eth0RxErr, eth0RxVal) = iddrx1f eth0RxClk' resetGen eth0RxCtl'
    (eth0RxData1, eth0RxData2) = iddrx1f eth0RxClk' resetGen eth0RxData'

    -- rgmii component
    (eth0TxErr, eth0TxEn, eth0TxData1, eth0TxData2) = rgmiiSender macOutput
    macInput = rgmiiReceiver eth0RxErr eth0RxVal eth0RxData1 eth0RxData2

    -- multiplex signals
    eth0TxCtl = oddrx1f eth0TxClk resetGen eth0TxErr eth0TxEn
    eth0TxData = oddrx1f eth0TxClk resetGen eth0TxData1 eth0TxData2

    -- delay output signals
    eth0TxClk' = delayf _ _ _ eth0TxClk
    eth0TxCtl' = delayf _ _ _ eth0TxCtl
    eth0TxData' = delayf _ _ _ eth0TxData

    -- clock forwarding
    eth0TxClk'' = oddrx1f eth0TxClk' resetGen (pure 1) (pure 0)

    {- SETUP MAC LAYER -}
    macOutput = pure 0 -- TODO

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
      , RGMIIChannel  -- eth0
          { rgmii_clk = rgmii_clk eth0_rx
          , rgmii_ctl = rgmii_ctl eth0_rx
          , rgmii_data = rgmii_data eth0_rx
          }
      , RGMIIChannel  --eth1
          { rgmii_clk = rgmii_clk eth1_rx
          , rgmii_ctl = rgmii_ctl eth1_rx
          , rgmii_data = rgmii_data eth1_rx
          }
      , HubOut
          { hub_clk = pure 0
          , hub_line_select = pure 0
          , hub_latch = pure 0
          , hub_output_enable = pure 0
          , hub_data = pure 0
          }
      )

makeTopEntity 'topEntity
