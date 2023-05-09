{-# language NumericUnderscores #-}

module Clash.Cores.Ethernet.Frame ( testFrame ) where

import Clash.Prelude
import Data.Maybe ( isNothing )

type Byte = BitVector 8

-- | ARP packet captured using Wireshark
--
-- To get the CRC as well while capturing use `ethtool -K IFNAME rx-fcs on`.
-- To receive even packets with a wrong CRC use `ethtool -K IFNAME rx-all on`.
testFrame :: Vec (8+64) Byte
testFrame = preamble ++ (hexToVec 0xffffffffffff54833a354a500806000108000604000154833a354a50c0a80101ffffffffffffc0a801c500000000000000000000000000000000000026544e8b)

hexToVec :: KnownNat n => Unsigned (8*n) -> Vec n Byte
hexToVec x = unpack $ pack x

preamble :: Vec 8 Byte
preamble = replicate d7 0x55 :< sfd
    where
        sfd = 0xd5
