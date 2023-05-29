module Clash.Cores.Ethernet.Stream
( mealyToCircuit
, SingleByteStream
, SingleByteStreamFwd
, FourByteStream
, FourByteStreamFwd
, EthernetHeader (..)
, TaggedSingleByteStream
, TaggedSingleByteStreamFwd
, TaggedStream
, TaggedStreamFwd
) where


import Clash.Prelude
import qualified Clash.Prelude as C
import Protocols
import Protocols.Axi4.Stream
import Protocols.Df


type Byte = Unsigned 8

-- | An untagged 4-byte wide stream
type FourByteStream (dom :: Domain) = Axi4Stream dom ('Axi4StreamConfig 4 0 0) ()
type FourByteStreamFwd = Maybe (Axi4StreamM2S ('Axi4StreamConfig 4 0 0) ())

-- | An untagged 1-byte wide stream
type SingleByteStream (dom :: Domain) = Axi4Stream dom ('Axi4StreamConfig 1 0 0) ()
type SingleByteStreamFwd = Maybe (Axi4StreamM2S ('Axi4StreamConfig 1 0 0) ())

data EthernetHeader = EthernetHeader { destinationMAC :: Vec 6 Byte
                                     , sourceMAC :: Vec 6 Byte
                                     , etherType :: BitVector 16 }
  deriving (Show, Eq, Generic, NFDataX)
-- | A tagged 4-byte wide stream, used for routing
type TaggedStream (dom :: Domain) = Axi4Stream dom ('Axi4StreamConfig 4 0 0) EthernetHeader
type TaggedStreamFwd = Maybe (Axi4StreamM2S ('Axi4StreamConfig 4 0 0) EthernetHeader)
-- | A tagged 1-byte wide stream
type TaggedSingleByteStream (dom :: Domain) = Axi4Stream dom ('Axi4StreamConfig 1 0 0) EthernetHeader
type TaggedSingleByteStreamFwd = Maybe (Axi4StreamM2S ('Axi4StreamConfig 1 0 0) EthernetHeader)


mealyToCircuit :: C.HiddenClockResetEnable dom
    => NFDataX a
    => (a -> (Maybe (Axi4StreamM2S conf1 user1), Axi4StreamS2M) -> (a, (Axi4StreamS2M, Maybe (Axi4StreamM2S conf2 user2))))
    -> a
    -> Circuit (Axi4Stream dom conf1 user1) (Axi4Stream dom conf2 user2)
mealyToCircuit machineAsFunction initialState = Circuit $ circuitFunction where
  circuitFunction = C.unbundle
                  . C.mealy machineAsFunction initialState
                  . C.bundle
