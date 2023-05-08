module Clash.Cores.Ethernet.MAC ( txMACCircuit, rxMACCircuit ) where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream

import Clash.Cores.Ethernet.CDC ( circuitCDC )
import Clash.Cores.Ethernet.Utils ( downconverter )
import Clash.Cores.Ethernet.Stream ( ifgEnforcer, preambleInserter )

type AxiStream (dom :: Domain) = Axi4Stream dom ('Axi4StreamConfig 4 0 0) ()
type AxiSingleStream (dom :: Domain) = Axi4Stream dom ('Axi4StreamConfig 1 0 0) ()

txMACCircuit :: forall (edom        :: Domain)
                       (mdom        :: Domain)
   . (KnownDomain edom, KnownDomain mdom)
  => Clock edom
  -> Reset edom
  -> Enable edom
  -> Clock mdom
  -> Reset mdom
  -> Enable mdom
  -> Circuit (AxiStream mdom) (AxiSingleStream edom)
txMACCircuit ethClk ethRst ethEn clk rst en = withEth $ ifgEnforcer <| preambleInserter <| downconverter <| circuitCDC clk ethClk rst ethRst en ethEn
  where
    withEth = withClockResetEnable ethClk resetGen enableGen

rxMACCircuit :: forall (edom        :: Domain)
                       (mdom        :: Domain)
   . (KnownDomain edom, KnownDomain mdom)
  => Clock edom
  -> Reset edom
  -> Enable edom
  -> Clock mdom
  -> Reset mdom
  -> Enable mdom
  -> Circuit (AxiSingleStream edom) (AxiStream mdom)
rxMACCircuit ethClk ethRst ethEn clk rst en = circuitCDC ethClk clk ethRst rst ethEn en <| undefined
