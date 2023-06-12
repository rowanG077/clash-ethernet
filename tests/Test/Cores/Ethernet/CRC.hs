module Test.Cores.Ethernet.CRC where

import Prelude
import Clash.Prelude
import Clash.Cores.Ethernet.CRC

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH

import Hedgehog ( (===) )
import qualified Hedgehog as H

-- Complete packets and corresponding checksum via https://packetor.com/

full_pack1 :: Vec 96 Byte
full_pack1 = unpack 0b100000000000000001111010000000000000000011010110000000000000000111000110000000001001000001011101000100000000000010100010000000000000000001001010101000101111100100000000000000001000000010001000000010110100011100000011000101010100000001100000000001110000000000000000110101101000000011111100100000001111110000000000011111000000000000000000010010000100000000000000011011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011100011011111111111111110000000010010000010111010000000010000000011110010001001010100000111100000000000000000000101000101000110110001000100010100010000001001110100111111000001100000000000000000000000000000000000000000000000000000000000000000000000000000000

checksum1 :: BitVector 32
checksum1 = 0b01000111000010001000101110100100

full_pack2 :: Vec 79 Byte
full_pack2 = unpack 0b00000000000000001000000000000000000000001000000000000000000010000010100100000000000000000100000000010000000000001010001000000000000000001011110000000000011100000000000000000000010100001000100011110100010100100000001110101010100000000100000000000011000000000000000010000000001000000000000001110000000100110000000010010100010011101000110000000100001000101010000010000100000000000000000000000000100000000000000000000000000000000000000000000000111100000100001000000010000000001111000001000010000000100000000000000000000000000000000010000000100100000100000011001110101001101100011001001110101001100010111001011111110111101001111000111000

checksum2 :: BitVector 32
checksum2 = 0b00111000111110110010001010000100

full_pack3 :: Vec 142 Byte
full_pack3 = unpack 0b00000000000001110011100000111100111010000100001100000000111110001100110010011011100000010000011000010000000000001010001000000000000000000000000100000000000000000000001000000000000000101000100000100100101010100101000001010000100000001000000001010000010100001000000000100000000000001010110011011011011001100000000000110110101101001011010010011110011010101000000100000001000000001000000000000000010000000000000001000000000000000000000000100000101101101000011010010110001101100001000000001110100001100010111001001110100101101111011000101110110011100100000010010110011101100000000000000000100000000000000010000000000000110011000000000000101000000000000010000000000000000000000001010100110100100000000001000000000000111000100000000011100010000000000010000000000000001000000000000000000000000101010000110010000000000010000001010010101011000011000110011001000000111000100000000000010000000000000010000000000000001000000011000010001100010000000001100000110000000111011011001110010011000000001110001000000000111000100000000000010000000000000010000000000000001000000011000010001100010000000001100000110000000111011011001110100011000000001110001000

checksum3 :: BitVector 32
checksum3 = 0b11111011110011101100001110101000

prop_crcBuffered1 :: H.Property
prop_crcBuffered1 = H.property $ calc_crc_buffered full_pack1  === checksum1

prop_crcBuffered2 :: H.Property
prop_crcBuffered2 = H.property $ calc_crc_buffered full_pack2 === checksum2

prop_crcBuffered3 :: H.Property
prop_crcBuffered3 = H.property $ calc_crc_buffered full_pack3 === checksum3

prop_crcStream1 :: H.Property
prop_crcStream1 = H.property $ calc_crc full_pack1  === checksum1

prop_crcStream2 :: H.Property
prop_crcStream2 = H.property $ calc_crc full_pack2 === checksum2

prop_crcStream3 :: H.Property
prop_crcStream3 = H.property $ calc_crc full_pack3 === checksum3

-- Update tests (generated via results from crc-calculation.py).

prop_CrcByteUpd00 :: H.Property
prop_CrcByteUpd00 = H.property $ update_crc_byte (0b01111111111111111000010111111111 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b00100111011111100101111101011010 :: BitVector 32)
prop_CrcByteUpd01 :: H.Property
prop_CrcByteUpd01 = H.property $ update_crc_byte (0b00100111011111100101111101011010 :: BitVector 32) (0b11010110 :: BitVector 8) === (0b11111000001110111011110000110011 :: BitVector 32)
prop_CrcByteUpd02 :: H.Property
prop_CrcByteUpd02 = H.property $ update_crc_byte (0b11111000001110111011110000110011 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b10010100000011000010001110110001 :: BitVector 32)
prop_CrcByteUpd03 :: H.Property
prop_CrcByteUpd03 = H.property $ update_crc_byte (0b10010100000011000010001110110001 :: BitVector 32) (0b00000001 :: BitVector 8) === (0b00111010001110101111110001000011 :: BitVector 32)
prop_CrcByteUpd04 :: H.Property
prop_CrcByteUpd04 = H.property $ update_crc_byte (0b00111010001110101111110001000011 :: BitVector 32) (0b11000110 :: BitVector 8) === (0b11000001010001001111100010000000 :: BitVector 32)
prop_CrcByteUpd05 :: H.Property
prop_CrcByteUpd05 = H.property $ update_crc_byte (0b11000001010001001111100010000000 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b00011101101100110000110100101110 :: BitVector 32)
prop_CrcByteUpd06 :: H.Property
prop_CrcByteUpd06 = H.property $ update_crc_byte (0b00011101101100110000110100101110 :: BitVector 32) (0b10010000 :: BitVector 8) === (0b11001110110100010111001100110011 :: BitVector 32)
prop_CrcByteUpd07 :: H.Property
prop_CrcByteUpd07 = H.property $ update_crc_byte (0b11001110110100010111001100110011 :: BitVector 32) (0b01011101 :: BitVector 8) === (0b10110000011101110000001111001110 :: BitVector 32)
prop_CrcByteUpd08 :: H.Property
prop_CrcByteUpd08 = H.property $ update_crc_byte (0b10110000011101110000001111001110 :: BitVector 32) (0b00010000 :: BitVector 8) === (0b11001010001111010100001101101110 :: BitVector 32)
prop_CrcByteUpd09 :: H.Property
prop_CrcByteUpd09 = H.property $ update_crc_byte (0b11001010001111010100001101101110 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b01001111010000110010100001001111 :: BitVector 32)
prop_CrcByteUpd10 :: H.Property
prop_CrcByteUpd10 = H.property $ update_crc_byte (0b01001111010000110010100001001111 :: BitVector 32) (0b10100010 :: BitVector 8) === (0b01001111111000011000001001101000 :: BitVector 32)
prop_CrcByteUpd11 :: H.Property
prop_CrcByteUpd11 = H.property $ update_crc_byte (0b01001111111000011000001001101000 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b11101101010010111010010111001010 :: BitVector 32)
prop_CrcByteUpd12 :: H.Property
prop_CrcByteUpd12 = H.property $ update_crc_byte (0b11101101010010111010010111001010 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b10111111110000010110101010101010 :: BitVector 32)
prop_CrcByteUpd13 :: H.Property
prop_CrcByteUpd13 = H.property $ update_crc_byte (0b10111111110000010110101010101010 :: BitVector 32) (0b01001010 :: BitVector 8) === (0b01000100000110111001101010001001 :: BitVector 32)
prop_CrcByteUpd14 :: H.Property
prop_CrcByteUpd14 = H.property $ update_crc_byte (0b01000100000110111001101010001001 :: BitVector 32) (0b10100010 :: BitVector 8) === (0b00111100000110001000111100001001 :: BitVector 32)
prop_CrcByteUpd15 :: H.Property
prop_CrcByteUpd15 = H.property $ update_crc_byte (0b00111100000110001000111100001001 :: BitVector 32) (0b11111001 :: BitVector 8) === (0b11111001101100011111111100001101 :: BitVector 32)
prop_CrcByteUpd16 :: H.Property
prop_CrcByteUpd16 = H.property $ update_crc_byte (0b11111001101100011111111100001101 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b00011010100011100000000000000110 :: BitVector 32)
prop_CrcByteUpd17 :: H.Property
prop_CrcByteUpd17 = H.property $ update_crc_byte (0b00011010100011100000000000000110 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b11101101100110110000101110100110 :: BitVector 32)
prop_CrcByteUpd18 :: H.Property
prop_CrcByteUpd18 = H.property $ update_crc_byte (0b11101101100110110000101110100110 :: BitVector 32) (0b10000000 :: BitVector 8) === (0b01101111011011110000011000101010 :: BitVector 32)
prop_CrcByteUpd19 :: H.Property
prop_CrcByteUpd19 = H.property $ update_crc_byte (0b01101111011011110000011000101010 :: BitVector 32) (0b10001000 :: BitVector 8) === (0b11111011111011000101000110100010 :: BitVector 32)
prop_CrcByteUpd20 :: H.Property
prop_CrcByteUpd20 = H.property $ update_crc_byte (0b11111011111011000101000110100010 :: BitVector 32) (0b00001011 :: BitVector 8) === (0b01001110101000101001010001100011 :: BitVector 32)
prop_CrcByteUpd21 :: H.Property
prop_CrcByteUpd21 = H.property $ update_crc_byte (0b01001110101000101001010001100011 :: BitVector 32) (0b01000111 :: BitVector 8) === (0b10101010100111001011001100111010 :: BitVector 32)
prop_CrcByteUpd22 :: H.Property
prop_CrcByteUpd22 = H.property $ update_crc_byte (0b10101010100111001011001100111010 :: BitVector 32) (0b00000011 :: BitVector 8) === (0b01000010000101101011101011011011 :: BitVector 32)
prop_CrcByteUpd23 :: H.Property
prop_CrcByteUpd23 = H.property $ update_crc_byte (0b01000010000101101011101011011011 :: BitVector 32) (0b00010101 :: BitVector 8) === (0b00101011101111101001000000001100 :: BitVector 32)
prop_CrcByteUpd24 :: H.Property
prop_CrcByteUpd24 = H.property $ update_crc_byte (0b00101011101111101001000000001100 :: BitVector 32) (0b01000000 :: BitVector 8) === (0b00001101111110000111000111000001 :: BitVector 32)
prop_CrcByteUpd25 :: H.Property
prop_CrcByteUpd25 = H.property $ update_crc_byte (0b00001101111110000111000111000001 :: BitVector 32) (0b01100000 :: BitVector 8) === (0b11001001101111000100011110110011 :: BitVector 32)
prop_CrcByteUpd26 :: H.Property
prop_CrcByteUpd26 = H.property $ update_crc_byte (0b11001001101111000100011110110011 :: BitVector 32) (0b00000111 :: BitVector 8) === (0b11000011000001001101001110010001 :: BitVector 32)
prop_CrcByteUpd27 :: H.Property
prop_CrcByteUpd27 = H.property $ update_crc_byte (0b11000011000001001101001110010001 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b01010100000110100010011101000000 :: BitVector 32)
prop_CrcByteUpd28 :: H.Property
prop_CrcByteUpd28 = H.property $ update_crc_byte (0b01010100000110100010011101000000 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b01110001101101001001110111011011 :: BitVector 32)
prop_CrcByteUpd29 :: H.Property
prop_CrcByteUpd29 = H.property $ update_crc_byte (0b01110001101101001001110111011011 :: BitVector 32) (0b11010110 :: BitVector 8) === (0b01010000111010001101101110000110 :: BitVector 32)
prop_CrcByteUpd30 :: H.Property
prop_CrcByteUpd30 = H.property $ update_crc_byte (0b01010000111010001101101110000110 :: BitVector 32) (0b10000000 :: BitVector 8) === (0b10010000010011000010110110000111 :: BitVector 32)
prop_CrcByteUpd31 :: H.Property
prop_CrcByteUpd31 = H.property $ update_crc_byte (0b10010000010011000010110110000111 :: BitVector 32) (0b11111100 :: BitVector 8) === (0b01101001001100001011110001100010 :: BitVector 32)
prop_CrcByteUpd32 :: H.Property
prop_CrcByteUpd32 = H.property $ update_crc_byte (0b01101001001100001011110001100010 :: BitVector 32) (0b10000000 :: BitVector 8) === (0b10111110110100000101010000011000 :: BitVector 32)
prop_CrcByteUpd33 :: H.Property
prop_CrcByteUpd33 = H.property $ update_crc_byte (0b10111110110100000101010000011000 :: BitVector 32) (0b11111100 :: BitVector 8) === (0b01010001111001000011010110001000 :: BitVector 32)
prop_CrcByteUpd34 :: H.Property
prop_CrcByteUpd34 = H.property $ update_crc_byte (0b01010001111001000011010110001000 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b10011000011000110011111010110000 :: BitVector 32)
prop_CrcByteUpd35 :: H.Property
prop_CrcByteUpd35 = H.property $ update_crc_byte (0b10011000011000110011111010110000 :: BitVector 32) (0b01111100 :: BitVector 8) === (0b01100000001010110110011001011010 :: BitVector 32)
prop_CrcByteUpd36 :: H.Property
prop_CrcByteUpd36 = H.property $ update_crc_byte (0b01100000001010110110011001011010 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b10000111110000111001110010010111 :: BitVector 32)
prop_CrcByteUpd37 :: H.Property
prop_CrcByteUpd37 = H.property $ update_crc_byte (0b10000111110000111001110010010111 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b10110100110101110010011111101011 :: BitVector 32)
prop_CrcByteUpd38 :: H.Property
prop_CrcByteUpd38 = H.property $ update_crc_byte (0b10110100110101110010011111101011 :: BitVector 32) (0b01001000 :: BitVector 8) === (0b01111001000111010001000011101010 :: BitVector 32)
prop_CrcByteUpd39 :: H.Property
prop_CrcByteUpd39 = H.property $ update_crc_byte (0b01111001000111010001000011101010 :: BitVector 32) (0b01000000 :: BitVector 8) === (0b11011111011011010000011110101000 :: BitVector 32)
prop_CrcByteUpd40 :: H.Property
prop_CrcByteUpd40 = H.property $ update_crc_byte (0b11011111011011010000011110101000 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b01000100110100110101111001010100 :: BitVector 32)
prop_CrcByteUpd41 :: H.Property
prop_CrcByteUpd41 = H.property $ update_crc_byte (0b01000100110100110101111001010100 :: BitVector 32) (0b01101100 :: BitVector 8) === (0b11110100110111000101001011000111 :: BitVector 32)
prop_CrcByteUpd42 :: H.Property
prop_CrcByteUpd42 = H.property $ update_crc_byte (0b11110100110111000101001011000111 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b01000110111011100100110011010101 :: BitVector 32)
prop_CrcByteUpd43 :: H.Property
prop_CrcByteUpd43 = H.property $ update_crc_byte (0b01000110111011100100110011010101 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b11000000010011001110100011000101 :: BitVector 32)
prop_CrcByteUpd44 :: H.Property
prop_CrcByteUpd44 = H.property $ update_crc_byte (0b11000000010011001110100011000101 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b00010001011000100101010110011001 :: BitVector 32)
prop_CrcByteUpd45 :: H.Property
prop_CrcByteUpd45 = H.property $ update_crc_byte (0b00010001011000100101010110011001 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b00101010100001010101111111000111 :: BitVector 32)
prop_CrcByteUpd46 :: H.Property
prop_CrcByteUpd46 = H.property $ update_crc_byte (0b00101010100001010101111111000111 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b00110010111101101010011100110110 :: BitVector 32)
prop_CrcByteUpd47 :: H.Property
prop_CrcByteUpd47 = H.property $ update_crc_byte (0b00110010111101101010011100110110 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b00101011000101110110000011111110 :: BitVector 32)
prop_CrcByteUpd48 :: H.Property
prop_CrcByteUpd48 = H.property $ update_crc_byte (0b00101011000101110110000011111110 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b10100100000010001000001110000001 :: BitVector 32)
prop_CrcByteUpd49 :: H.Property
prop_CrcByteUpd49 = H.property $ update_crc_byte (0b10100100000010001000001110000001 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b11101010101010001010000111010010 :: BitVector 32)
prop_CrcByteUpd50 :: H.Property
prop_CrcByteUpd50 = H.property $ update_crc_byte (0b11101010101010001010000111010010 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b01000010100000100010001010101111 :: BitVector 32)
prop_CrcByteUpd51 :: H.Property
prop_CrcByteUpd51 = H.property $ update_crc_byte (0b01000010100000100010001010101111 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b10111111001001101110010000011001 :: BitVector 32)
prop_CrcByteUpd52 :: H.Property
prop_CrcByteUpd52 = H.property $ update_crc_byte (0b10111111001001101110010000011001 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b10100011100101010010100111000011 :: BitVector 32)
prop_CrcByteUpd53 :: H.Property
prop_CrcByteUpd53 = H.property $ update_crc_byte (0b10100011100101010010100111000011 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b01101001010001011011001111010111 :: BitVector 32)
prop_CrcByteUpd54 :: H.Property
prop_CrcByteUpd54 = H.property $ update_crc_byte (0b01101001010001011011001111010111 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b11001011110111111110000110011000 :: BitVector 32)
prop_CrcByteUpd55 :: H.Property
prop_CrcByteUpd55 = H.property $ update_crc_byte (0b11001011110111111110000110011000 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b10101001001000001100001111111000 :: BitVector 32)
prop_CrcByteUpd56 :: H.Property
prop_CrcByteUpd56 = H.property $ update_crc_byte (0b10101001001000001100001111111000 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b11110011001001010101111000000001 :: BitVector 32)
prop_CrcByteUpd57 :: H.Property
prop_CrcByteUpd57 = H.property $ update_crc_byte (0b11110011001001010101111000000001 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b10100001101001011101101011010000 :: BitVector 32)
prop_CrcByteUpd58 :: H.Property
prop_CrcByteUpd58 = H.property $ update_crc_byte (0b10100001101001011101101011010000 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b01010000001101001001101110111001 :: BitVector 32)
prop_CrcByteUpd59 :: H.Property
prop_CrcByteUpd59 = H.property $ update_crc_byte (0b01010000001101001001101110111001 :: BitVector 32) (0b00000001 :: BitVector 8) === (0b01001100000011000001001000000110 :: BitVector 32)
prop_CrcByteUpd60 :: H.Property
prop_CrcByteUpd60 = H.property $ update_crc_byte (0b01001100000011000001001000000110 :: BitVector 32) (0b11000110 :: BitVector 8) === (0b00001101100110001110110111010101 :: BitVector 32)
prop_CrcByteUpd61 :: H.Property
prop_CrcByteUpd61 = H.property $ update_crc_byte (0b00001101100110001110110111010101 :: BitVector 32) (0b11111111 :: BitVector 8) === (0b10101001001000000101001100101100 :: BitVector 32)
prop_CrcByteUpd62 :: H.Property
prop_CrcByteUpd62 = H.property $ update_crc_byte (0b10101001001000000101001100101100 :: BitVector 32) (0b11111111 :: BitVector 8) === (0b11110011101101011000101011111110 :: BitVector 32)
prop_CrcByteUpd63 :: H.Property
prop_CrcByteUpd63 = H.property $ update_crc_byte (0b11110011101101011000101011111110 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b00110001011100010010010111010000 :: BitVector 32)
prop_CrcByteUpd64 :: H.Property
prop_CrcByteUpd64 = H.property $ update_crc_byte (0b00110001011100010010010111010000 :: BitVector 32) (0b10010000 :: BitVector 8) === (0b10100001110101101010000010110111 :: BitVector 32)
prop_CrcByteUpd65 :: H.Property
prop_CrcByteUpd65 = H.property $ update_crc_byte (0b10100001110101101010000010110111 :: BitVector 32) (0b01011101 :: BitVector 8) === (0b00100011010011101111110011100100 :: BitVector 32)
prop_CrcByteUpd66 :: H.Property
prop_CrcByteUpd66 = H.property $ update_crc_byte (0b00100011010011101111110011100100 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b11011011100111000111010000111001 :: BitVector 32)
prop_CrcByteUpd67 :: H.Property
prop_CrcByteUpd67 = H.property $ update_crc_byte (0b11011011100111000111010000111001 :: BitVector 32) (0b10000000 :: BitVector 8) === (0b10100110101001001011100100001000 :: BitVector 32)
prop_CrcByteUpd68 :: H.Property
prop_CrcByteUpd68 = H.property $ update_crc_byte (0b10100110101001001011100100001000 :: BitVector 32) (0b01111001 :: BitVector 8) === (0b01001111000100000001001111000101 :: BitVector 32)
prop_CrcByteUpd69 :: H.Property
prop_CrcByteUpd69 = H.property $ update_crc_byte (0b01001111000100000001001111000101 :: BitVector 32) (0b00010010 :: BitVector 8) === (0b00011100110110100000100011011000 :: BitVector 32)
prop_CrcByteUpd70 :: H.Property
prop_CrcByteUpd70 = H.property $ update_crc_byte (0b00011100110110100000100011011000 :: BitVector 32) (0b10100000 :: BitVector 8) === (0b10100011000101011001100010110100 :: BitVector 32)
prop_CrcByteUpd71 :: H.Property
prop_CrcByteUpd71 = H.property $ update_crc_byte (0b10100011000101011001100010110100 :: BitVector 32) (0b11110000 :: BitVector 8) === (0b11101001111101001100010000100111 :: BitVector 32)
prop_CrcByteUpd72 :: H.Property
prop_CrcByteUpd72 = H.property $ update_crc_byte (0b11101001111101001100010000100111 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b00010011101001001111000101110110 :: BitVector 32)
prop_CrcByteUpd73 :: H.Property
prop_CrcByteUpd73 = H.property $ update_crc_byte (0b00010011101001001111000101110110 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b11100101101000111000101110101001 :: BitVector 32)
prop_CrcByteUpd74 :: H.Property
prop_CrcByteUpd74 = H.property $ update_crc_byte (0b11100101101000111000101110101001 :: BitVector 32) (0b10100010 :: BitVector 8) === (0b01110001111001111110010010110000 :: BitVector 32)
prop_CrcByteUpd75 :: H.Property
prop_CrcByteUpd75 = H.property $ update_crc_byte (0b01110001111001111110010010110000 :: BitVector 32) (0b10001101 :: BitVector 8) === (0b00000011100100011011000011011101 :: BitVector 32)
prop_CrcByteUpd76 :: H.Property
prop_CrcByteUpd76 = H.property $ update_crc_byte (0b00000011100100011011000011011101 :: BitVector 32) (0b10001000 :: BitVector 8) === (0b10011100111100111111101101010001 :: BitVector 32)
prop_CrcByteUpd77 :: H.Property
prop_CrcByteUpd77 = H.property $ update_crc_byte (0b10011100111100111111101101010001 :: BitVector 32) (0b10001010 :: BitVector 8) === (0b11100011111010101111000101110000 :: BitVector 32)
prop_CrcByteUpd78 :: H.Property
prop_CrcByteUpd78 = H.property $ update_crc_byte (0b11100011111010101111000101110000 :: BitVector 32) (0b00100000 :: BitVector 8) === (0b00100010000110110111000010000000 :: BitVector 32)
prop_CrcByteUpd79 :: H.Property
prop_CrcByteUpd79 = H.property $ update_crc_byte (0b00100010000110110111000010000000 :: BitVector 32) (0b01001110 :: BitVector 8) === (0b10001010110100010000110111000000 :: BitVector 32)
prop_CrcByteUpd80 :: H.Property
prop_CrcByteUpd80 = H.property $ update_crc_byte (0b10001010110100010000110111000000 :: BitVector 32) (0b10011111 :: BitVector 8) === (0b10010111100010111111011010100111 :: BitVector 32)
prop_CrcByteUpd81 :: H.Property
prop_CrcByteUpd81 = H.property $ update_crc_byte (0b10010111100010111111011010100111 :: BitVector 32) (0b10000011 :: BitVector 8) === (0b10110000101011001100110000011000 :: BitVector 32)
prop_CrcByteUpd82 :: H.Property
prop_CrcByteUpd82 = H.property $ update_crc_byte (0b10110000101011001100110000011000 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b00010001111100101001010101111110 :: BitVector 32)
prop_CrcByteUpd83 :: H.Property
prop_CrcByteUpd83 = H.property $ update_crc_byte (0b00010001111100101001010101111110 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b10111010010001011011100011000111 :: BitVector 32)
prop_CrcByteUpd84 :: H.Property
prop_CrcByteUpd84 = H.property $ update_crc_byte (0b10111010010001011011100011000111 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b11010111000011001001110010101000 :: BitVector 32)
prop_CrcByteUpd85 :: H.Property
prop_CrcByteUpd85 = H.property $ update_crc_byte (0b11010111000011001001110010101000 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b00000011010000001011001111101100 :: BitVector 32)
prop_CrcByteUpd86 :: H.Property
prop_CrcByteUpd86 = H.property $ update_crc_byte (0b00000011010000001011001111101100 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b01001101111100001100101011011001 :: BitVector 32)
prop_CrcByteUpd87 :: H.Property
prop_CrcByteUpd87 = H.property $ update_crc_byte (0b01001101111100001100101011011001 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b11110101100000010010111110100100 :: BitVector 32)
prop_CrcByteUpd88 :: H.Property
prop_CrcByteUpd88 = H.property $ update_crc_byte (0b11110101100000010010111110100100 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b00011111010100100011001001100010 :: BitVector 32)
prop_CrcByteUpd89 :: H.Property
prop_CrcByteUpd89 = H.property $ update_crc_byte (0b00011111010100100011001001100010 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b00100110011011000000010011001101 :: BitVector 32)
prop_CrcByteUpd90 :: H.Property
prop_CrcByteUpd90 = H.property $ update_crc_byte (0b00100110011011000000010011001101 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b11101110101000010011011001010010 :: BitVector 32)
prop_CrcByteUpd91 :: H.Property
prop_CrcByteUpd91 = H.property $ update_crc_byte (0b11101110101000010011011001010010 :: BitVector 32) (0b00000000 :: BitVector 8) === (0b01011000000100011101010001110011 :: BitVector 32)

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
