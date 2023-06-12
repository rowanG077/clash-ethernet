{-|
Module      : Clash.Cores.Ethernet.CRC
Description : CRC Calculation for Ethernet Frames
Maintainer  : lucas@qbaylogic.com

This module provides functions for calculating CRC (Cyclic Redundancy Check) for Ethernet frames.
The CRC calculation is performed on bytes of input data and produces a 32-bit CRC value.
For more information on CRC see Clause 3.2.9. File is available behind a pay wall.
A free draft version is available here: https://www.ieee802.org/3/as/public/0503/3d0_1_CMP.pdf#page=6
-}

module Clash.Cores.Ethernet.CRC where

import Clash.Prelude

-- | A type synonym for 'BitVector' 8.
type Byte = BitVector 8

-- | The state of the CRC calculation, consisting of a 32-bit 'BitVector' and an index.
-- The index is used to keep track of the first four bytes of input.
type CRCState = (Vec 4 Byte, Index 5)

-- | The starting state of the CRC calculation.
crc_starting_state :: CRCState
crc_starting_state = (repeat 0, 0)

-- Bit representation of x^32 + x^26 + x^23 + x^22 + x^16 + x^12 + x^11 + x^10 + x^8 + x^7 + x^5 + x^4 + x^2 + x + 1
crc_polynomial :: BitVector 33
crc_polynomial = 0b100000100110000010001110110110111
-- first bit does not matter, because msb gets dropped anyways.

-- | Update the 32-bit CRC based on one bit of input.
-- The new bit is appended to the CRC and the result is calculated modulo the polynomial.
update_crc_bit :: BitVector 32 -> Bit -> BitVector 32
update_crc_bit crc b
  | msb crc == 1 = resize $ xor app crc_polynomial
  | otherwise    = resize app
 where
  app = pack (crc, b)

-- | Update the 32-bit CRC based on one byte of input.
update_crc_byte :: BitVector 32 -> Byte -> BitVector 32
update_crc_byte crc new_b = foldl update_crc_bit crc (unpack new_b)

-- | Update the 'CRCState' based on one 'Byte' of input.
-- If the index of the CRC state is four, then the first four input bytes have been handled.
-- Otherwise, the incoming byte is handled as one of the first four bytes.
update_crc_state :: CRCState -> Byte -> CRCState
update_crc_state (crc, i) byte
  | i == 4    = (unpack $ update_crc_byte (pack crc) byte, 4)
  | otherwise = (replace i (complement byte) crc, succ i)

-- | Perform the final steps after the input is done.
-- Multiply the CRC with x^32 and complement the result.
finish_crc_state :: CRCState -> BitVector 32
finish_crc_state (crc, _) = complement $
  foldl update_crc_byte (pack crc) (replicate d4 0)

-- | Calculate the CRC for at least 4 bytes of input.
calc_crc_buffered :: forall n . KnownNat n => Vec (n+4) Byte -> BitVector 32
calc_crc_buffered input = complement res
 where
  (start, rest) = unpack (pack input)
  res = foldl update_crc_byte (complement start) (rest ++ (replicate d4 0 :: Vec 4 Byte))

-- | Calculate the CRC for at least 4 bytes of input using the CRC state.
calc_crc :: forall n . KnownNat n => Vec (n+4) Byte -> BitVector 32
calc_crc input = finish_crc_state final_state
 where
  final_state = foldl update_crc_state crc_starting_state input
