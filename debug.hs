{-# LANGUAGE PartialTypeSignatures #-}

module MyDebug (sampleTopEntityN, packet) where

import Clash.Lattice.ECP5.Colorlight.TopEntity (topEntity)
import Clash.Lattice.ECP5.Colorlight.CRG
import Clash.Cores.Ethernet.RGMII
import Clash.Prelude
import qualified Data.List as L

packet = asBytes $ L.takeWhile (\(_,a,_) -> a == high) $ L.dropWhile (\(_,a,_) -> a == low) $ sampleTopEntityN 400000000
    where
        asBytes = L.map (\((_,_,l):(_,_,m):[]) -> m ++# l) . L.groupBy (\(a,_,_) (b,_, _) -> a == high && b == low)

-- Gives as many cycles of output as you want.
sampleTopEntityN :: Int -> [_]
sampleTopEntityN n = simulateN n (lens . wrapper . unbundle) $ L.zip (L.repeat low) (L.repeat 0)

-- Displays only the output data we are interested it, needs to present a `Signal dom generic_output_data`
lens :: _ -> Signal dom (Bit, Bit, BitVector 4)
lens (_, _, _, eth0_tx, _, _) = bundle (rgmii_tx_clk eth0_tx, rgmii_tx_ctl eth0_tx, rgmii_tx_data eth0_tx)

-- Wraps the `topEntity` function so that it receives the input data we give.
-- Has bogus inputs for anything unneeded.
wrapper :: (Signal DomDDREth0 Bit, Signal DomDDREth0 (BitVector 4)) -> _
wrapper (ctl, dat) = topEntity clockGen uart sdram mdio eth0_rx eth1_rx
    where
        mdio = undefined
        sdram = undefined
        uart = undefined
        eth0_rx = RGMIIRXChannel { rgmii_rx_clk = clockGen
                              , rgmii_rx_ctl = ctl
                              , rgmii_rx_data = dat
                              }
        eth1_rx = RGMIIRXChannel { rgmii_rx_clk = clockGen
                              , rgmii_rx_ctl = pure 0
                              , rgmii_rx_data = pure 0
                              }

-- myTest :: [(Maybe Bit, Maybe MDIOResponse)]
-- myTest = simulate @System (f . unbundle) input
--     where
--         f (a,b) = bundle $ mdioComponent (pure False) a b
--         input :: [(Bit, Maybe MDIORequest)]
--         input = [(low, Nothing), (low, Just $ MDIORead 1 3)] L.++ L.replicate 150 (low, Nothing)
