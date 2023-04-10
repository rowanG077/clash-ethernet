module Clash.Cores.Ethernet.Frame (sendFrameOnPulse, testFrame) where

import Clash.Prelude

import Data.Maybe (isNothing)

type Byte = BitVector 8

-- | ARP packet captured using Wireshark
--
-- To get the CRC as well while capturing use `ethtool -K IFNAME rx-fcs on`.
-- To receive even packets with a wrong CRC use `ethtool -K IFNAME rx-all on`.
testFrame :: Vec (8+64) Byte
testFrame = preamble ++ (hexToVec 0xffffffffffff54833a354a500806000108000604000154833a354a50c0a80101ffffffffffffc0a801c500000000000000000000000000000000000026544e8b)

preamble :: Vec 8 Byte
preamble = replicate d7 0x55 :< sfd
    where
        sfd = 0xd5

hexToVec :: KnownNat n => Unsigned (8*n) -> Vec n Byte
hexToVec x = unpack $ pack x

-- -- | Creates a static ethernet frame including preamble and CRC
-- --
-- -- NOTE:Does not include the inter-frame gap (IFG/IPG)
-- createStaticFrame ::
    -- Vec 6 Byte
    -- -> Vec 6 Byte
    -- -> Vec 2 Byte
    -- -> Vec n Byte
    -- -> Vec (8 + 6 + 6 + 2 + n + 4) Byte
-- createStaticFrame src dest eth_type payload = preamble ++ dest ++ src ++ eth_type ++ payload ++ crc
    -- where
        -- crc = crcCalculate $ dest ++ src ++ eth_type ++ payload

sendFrameOnPulse :: forall dom . HiddenClockResetEnable dom
    => Signal dom Bool
    -> Signal dom (Maybe Byte)
    -> Signal dom (Maybe Byte)
-- sendFrameOnPulse inp = liftA2 output counter mem
sendFrameOnPulse inp incoming = fmap change_byte $ bundle (incoming, counter, mem, sfd_register)
    where
        change_byte :: (Maybe Byte, Unsigned 32, Byte, Maybe Byte) -> (Maybe (BitVector 8))
        change_byte (i, c, n, r)
            | c == 0 = if isNothing i then Nothing else Just n
            | c < 72 = Just n
            | c == 72 = Nothing
            | otherwise = Nothing

        sfd_register :: Signal dom (Maybe Byte)
        sfd_register = register (Just 1) (upd <$> bundle (counter, sfd_register, incoming))
            where
                upd (c, reg, i)
                    | c == 7 = i
                    | otherwise = reg

        counter :: Signal dom (Unsigned 32)
        counter = register 0 (inc <$> bundle (counter, incoming))
            where
                inc (i,p)
                    | isNothing p = 0
                    | otherwise = i+1

        frame_length = 72
        output c m
            | c == 0 = Nothing
            | c <= frame_length = Just m
            | c <= frame_length+12 = Nothing
            | otherwise = Nothing

        -- counter :: Signal dom (Unsigned 32)
        -- counter = register 0 (inc <$> bundle (counter, inp))
            -- where
                -- inc (i,pulse)
                    -- -- | i == 0 = 1
                    -- | i == 0 && not pulse = 0
                    -- | i == 0 && pulse = 1
                    -- | i < frame_length+12 = i+1
                    -- | otherwise = 0

        initialState :: Vec 72 (BitVector 8)
        -- initialState = createStaticFrame srcMAC broadcastMAC
        initialState = testFrame
        -- broadcastMAC = replicate d6 0b11111111
        -- destMAC = replicate d6 0b1
        -- srcMAC = replicate d6 0b11

        mem = blockRam initialState readAddr write
        readAddr :: Signal dom (BitVector 32)
        readAddr = pack <$> (+1) <$> counter
        write = pure Nothing
