{-# language NumericUnderscores #-}

module Clash.Cores.Ethernet.Frame ( testFrame, testPayload, testHeader ) where

import Clash.Prelude
import Data.Maybe ( isNothing )
import Clash.Cores.Ethernet.Stream ( EthernetHeader (..) )

type Byte = BitVector 8

-- | ARP packet captured using Wireshark
--
-- To get the CRC as well while capturing use `ethtool -K IFNAME rx-fcs on`.
-- To receive even packets with a wrong CRC use `ethtool -K IFNAME rx-all on`.
testFrame :: Vec (8+64) Byte
testFrame = preamble ++ (hexToVec 0xffffffffffff54833a354a500806000108000604000154833a354a50c0a80101ffffffffffffc0a801c500000000000000000000000000000000000026544e8b)

testHeader :: EthernetHeader
testHeader = EthernetHeader { destinationMAC = unpack (0xffffffffffff :: BitVector (6*8))
                            , sourceMAC = unpack (0x54833a354a50 :: BitVector (6*8))
                            , etherType = 0x0806
                            }

testPayload :: Vec 28 Byte
testPayload = hexToVec 0x000108000604000154833a354a50c0a80101ffffffffffffc0a801c5

hexToVec :: KnownNat n => Unsigned (8*n) -> Vec n Byte
hexToVec x = unpack $ pack x

preamble :: Vec 8 Byte
preamble = replicate d7 0x55 :< sfd
    where
        sfd = 0xd5

sendFrameOnPulse :: forall dom . HiddenClockResetEnable dom
    => Signal dom Bool
    -> Signal dom (Maybe Byte)
sendFrameOnPulse inp = liftA2 output counter mem
    where
        frame_length = 72
        output c m
            | c == 0 = Nothing
            | c <= frame_length = Just m
            | c <= frame_length+12 = Nothing
            | otherwise = Nothing

        counter :: Signal dom (Unsigned 32)
        counter = register 0 (inc <$> bundle (counter, inp))
            where
                inc (i,pulse)
                    -- | i == 0 = 1
                    | i == 0 && not pulse = 0
                    | i == 0 && pulse = 1
                    | i < frame_length+12 = i+1
                    | otherwise = 0

        initialState :: Vec 72 (BitVector 8)
        -- initialState = testFrame
        initialState = createStaticFrame srcMAC broadcastMAC type_ipv4 payload
            where
                -- destMAC = replicate d6 0b1
                broadcastMAC = replicate d6 0xff
                srcMAC = replicate d6 0x03
                type_ipv4 = 0x0800
                payload = 0x42 :> replicate d45 0x00

        mem = blockRam initialState readAddr write
        readAddr :: Signal dom (BitVector 32)
        readAddr = pack <$> counter
        write = pure Nothing
