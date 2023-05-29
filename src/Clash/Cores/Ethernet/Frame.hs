{-# language NumericUnderscores #-}

module Clash.Cores.Ethernet.Frame ( testFrame, testPayload, testHeader ) where

import Clash.Prelude
import Clash.Cores.Ethernet.Stream ( EthernetHeader (..) )

-- | ARP packet captured using Wireshark
--
-- To get the CRC as well while capturing use `ethtool -K IFNAME rx-fcs on`.
-- To receive even packets with a wrong CRC use `ethtool -K IFNAME rx-all on`.
testFrame :: Vec (8+64) (Unsigned 8)
testFrame = preamble ++ (hexToVec 0xffffffffffff54833a354a500806000108000604000154833a354a50c0a80101ffffffffffffc0a801c500000000000000000000000000000000000026544e8b)

testHeader :: EthernetHeader
testHeader = EthernetHeader { destinationMAC = unpack (0xffffffffffff :: BitVector (6*8))
                            , sourceMAC = unpack (0x54833a354a50 :: BitVector (6*8))
                            , etherType = 0x0806
                            }

testPayload :: Vec 28 (Unsigned 8)
testPayload = hexToVec 0x000108000604000154833a354a50c0a80101ffffffffffffc0a801c5

hexToVec :: KnownNat n => Unsigned (8*n) -> Vec n (Unsigned 8)
hexToVec x = unpack $ pack x

preamble :: Vec 8 (Unsigned 8)
preamble = replicate d7 0x55 :< sfd
    where
        sfd = 0xd5
