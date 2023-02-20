module Clash.Cores.Ethernet.Bridge.Mdio (MDIOResponse, MDIORequest, mdioComponent) where

import Clash.Prelude

data MDIORequest
  = MDIORead (Unsigned 5) (Unsigned 5) -- PHY address, REG address
  | MDIOWrite (Unsigned 5) (Unsigned 5) (BitVector 16) -- PHY address, REG address, 16-bit data
  deriving (Show, Eq, Generic, NFDataX)

data MDIOResponse
  = MDIOReadResult (BitVector 16) -- contents of register that was read
  | MDIOWriteAck
  deriving (Show, Eq, Generic, NFDataX)


mdioComponent
  :: HiddenClockResetEnable dom
  => KnownDomain dom
  => Signal dom Bit -- eth_mdio in
  -> Signal dom (Maybe MDIORequest) -- receive a request from the bridge
  -> ( Signal dom (Maybe Bit) -- eth_mdio out
     , Signal dom (Maybe MDIOResponse)  -- Output result or ACK when we have it
     )
mdioComponent = undefined
