module Clash.Cores.Ethernet.CRC where

import Clash.Prelude

-- This file contains haskell code for calculating the CRC.
-- The CRCState is programmed so that it can be updated one byte at a time.
-- This way we do not need a buffer to store the entire input.
-- For more information on CRC see crc-calculation.py and/or Clause 3.2.9.

type Byte = BitVector 8
type CRCState = (BitVector 32, Index 5)

crc_starting_state :: CRCState
crc_starting_state = (0, 0) 

-- Bit representation of x^32 + x^26 + x^23 + x^22 + x^16 + x^12 + x^11 + x^10 + x^8 + x^7 + x^5 + x^4 + x^2 + x + 1
poly_bytes :: BitVector 33
poly_bytes = 0b100000100110000010001110110110111
-- first bit does not matter, because msb gets dropped anyways.

-- Update 32 bit CRC based on one bit of input.
upd_crc_bit :: BitVector 32 -> Bit -> BitVector 32
upd_crc_bit crc b = res where
    app :: BitVector 33
    app = crc ++# (pack b)
    (_ :: Bit, res :: BitVector 32) = unpack $ if msb crc == 1 then  xor app poly_bytes else app

-- Update 32 bit CRC based on one byte of input.
upd_crc_byte :: BitVector 32 -> Byte -> BitVector 32
upd_crc_byte crc new_b = foldl upd_crc_bit crc (unpack new_b)

-- Update CRCState based on one byte of input.
upd_crc_state :: CRCState -> Byte -> CRCState
upd_crc_state (crc, i) byte = if i < 4 then (crc', i + 1) else (upd_crc_byte crc byte, i) where
    (a, b, c, d) = unpack crc :: (Byte, Byte, Byte, Byte)
    crc' = case i of 
                0 -> (complement byte) ++# b ++# c ++# d
                1 -> a ++# (complement byte) ++# c ++# d
                2 -> a ++# b ++# (complement byte) ++# d
                3 -> a ++# b ++# c ++# (complement byte)
                _ -> error("upd_crc_state, crc': unexpected index.")

-- Final steps after input is done. Multiply with x^32 and complement the result.
finish_crc_state :: CRCState -> BitVector 32
finish_crc_state (crc, _) = complement res where
    res = foldl upd_crc_byte crc (unpack 0 :: Vec 4 Byte)

-- Calculate the CRC for >= 4 bytes of input.
calc_crc_buffered :: forall n . KnownNat n => Vec (n+4) Byte -> BitVector 32
calc_crc_buffered input = complement res where
    (start, rest) = unpack (pack input) :: (BitVector 32, Vec n Byte)
    res = foldl upd_crc_byte (complement start) (rest ++ (unpack 0 :: Vec 4 Byte))

-- Calculate the CRC for >= 4 bytes of input using crc_state.
calc_crc :: forall n . KnownNat n => Vec (n+4) Byte -> BitVector 32
calc_crc input = finish_crc_state final_state where
    final_state = foldl upd_crc_state crc_starting_state input
