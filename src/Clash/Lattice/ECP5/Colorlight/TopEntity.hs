module Clash.Lattice.ECP5.Colorlight.TopEntity (topEntity) where

import Clash.Annotations.TH
import Clash.Explicit.Prelude
import Clash.Lattice.ECP5.Colorlight.CRG
import Clash.Lattice.ECP5.Prims

import Clash.Cores.UART

-- TODD: First order of business is to clean up these input and outputs
-- into data types

topEntity
  :: "clk25" ::: Clock Dom25
  -> "uart_rx" ::: Signal Dom50 Bit
  -> "sdram_dq" ::: BiSignalIn 'Floating Dom50 32
  -> "eth_mdio" ::: BiSignalIn 'Floating Dom50 1
  -> "eth0_rx_clk" ::: Clock DomEth0
  -> "eth0_rx_ctl" ::: Signal DomEth0 Bit
  -> "eth0_rx_data" ::: Signal DomEth0 (BitVector 4)
  -> "eth1_rx_clk" ::: Clock DomEth1
  -> "eth1_rx_ctl" ::: Signal DomEth1 Bit
  -> "eth1_rx_data" ::: Signal DomEth1 (BitVector 4)
  -> ( "uart_tx" ::: Signal Dom50 Bit
     , "sdram_clock" ::: Clock Dom50
     , "sdram_a" ::: Signal Dom50 (BitVector 11)
     , "sdram_we_n" ::: Signal Dom50 Bit
     , "sdram_ras_n" ::: Signal Dom50 Bit
     , "sdram_cas_n" ::: Signal Dom50 Bit
     , "sdram_ba" ::: Signal Dom50 (BitVector 2)
     , "sdram_dq" ::: BiSignalOut 'Floating Dom50 32
     , "eth_mdio" ::: BiSignalOut 'Floating Dom50 1
     , "eth_mdc" ::: Signal Dom50 Bit
     , "eth_rst_n" ::: Signal Dom50 Bit
     , "eth0_tx_clk" ::: Clock DomEth0
     , "eth0_tx_ctl" ::: Signal DomEth0 Bit
     , "eth0_tx_data" ::: Signal DomEth0 (BitVector 4)
     , "eth1_tx_clk" ::: Clock DomEth1
     , "eth1_tx_ctl" ::: Signal DomEth1 Bit
     , "eth1_tx_data" ::: Signal DomEth1 (BitVector 4)
     , "hub_clk" ::: Signal Dom50 Bit
     , "hub_line_select" ::: Signal Dom50 (BitVector 5)
     , "hub_latch" ::: Signal Dom50 Bit
     , "hub_output_enable" ::: Signal Dom50 Bit
     , "hub_data" ::: Signal Dom50 (BitVector 48)
     )
topEntity clk25 uartRxBit dq_in mdio_in eth0RxClk _eth0RxCtl _eth0RxData eth1RxClk _eth1RxCtl _eth1RxData = result
  where
    (clk50, rst50) = crg clk25
    en50 = enableGen

    -- Simply echo back uart signals through IO flip flops
    uartTxBit = ofs1p3bx clk50 rst50 en50 $ ifs1p3bx clk50 rst50 en50 uartRxBit

    {- MDIO SETUP -}
    dq :: Signal Dom50 (BitVector 32)
    mdio :: Signal Dom50 Bit
    (dq_out, dq) = bb dq_in onoff (ofs1p3bx clk50 rst50 en50 dqReg) -- sdram_dq
    (mdio_out, mdio) = bb mdio_in onoff (ofs1p3bx clk50 rst50 en50 mdioReg) -- sdram_dq

    dqReg = ifs1p3bx clk50 rst50 en50 dq
    mdioReg = ifs1p3bx clk50 rst50 en50 mdio

    onoff = register clk50 rst50 en50 0 $ fmap complement onoff

    {- ETH0 ~ RGMII SETUP -}    
    -- generate tx_clk (125mHz)
    eth0TxClk = _

    -- delay input signals
    eth0RxClk' = delayf _ _ _ eth0RxClk
    eth0RxCtl' = delayf _ _ _ _eth0RxCtl
    eth0RxData' = delayf _ _ _ _eth0RxData

    -- demultiplex signal
    (eth0RxErr, eth0RxVal) = iddrx1f _ _ _ eth0RxCtl'
    (eth0RxData1, eth0RxData2) = iddrx1f  _ _ _ eth0RxData'

    -- rgmii component
    (eth0TxErr, eth0TxEn, eth0TxData1, eth0TxData2) = rgmiiSender macOutput
    macInput = rgmiiReceiver eth0RxErr eth0RxVal eth0RxData1 eth0RxData2

    -- multiplex signals
    eth0TxCtl = oddrx1f eth0TxClk _ eth0TxErr eth0TxEn
    eth0TxData = oddrx1f eth0TxClk _ eth0TxData1 eth0TxData2

    -- delay output signals
    eth0TxClk' = delayf _ _ _ eth0TxClk
    eth0TxCtl' = delayf _ _ _ eth0TxCtl
    eth0TxData' = delayf _ _ _ eth0TxData

    {- SETUP MAC LAYER -}
    macOutput = pure 0 -- TODO

    result =
      ( uartTxBit -- uart_tx
      , clk50 -- sdram_clock
      , pure 0 -- sdram_a
      , pure 1-- sdram_we_n
      , pure 1 -- sdram_ras_n
      , pure 1 -- sdram_cas_n
      , pure 0 -- sdram_ba
      , dq_out -- sdram_dq
      , mdio_out -- eth_mdio
      , pure 0 -- eth_mdc
      , pure 1 -- eth_rst_n
      , eth0TxClk' -- eth0_tx_clk
      , eth0TxCtl' -- eth0_tx_ctl
      , eth0TxData' -- eth0_tx_data
      , eth1RxClk -- eth1_tx_clk
      , pure 0 -- eth1_tx_ctl
      , pure 0 -- eth1_tx_data
      , pure 0 -- hub_clk
      , pure 0 -- hub_line_select
      , pure 0 -- hub_latch
      , pure 0 -- hub_output_enable
      , pure 0 -- hub_data
      )

makeTopEntity 'topEntity
