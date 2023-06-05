{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -fplugin=Protocols.Plugin  #-}

module Clash.Cores.Ethernet.CRC where

import Clash.Prelude as C
import Data.Proxy

import Protocols
import Protocols.Axi4.Stream
import Protocols.Internal ( fromSignals )
import Protocols.DfConv ( fifo )


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


data PacketState = Idle | PacketValid | PacketInvalid
    deriving (Show, ShowX, Eq, Generic, NFDataX)

-- | Fifo that adds if crc of stream is ok
crcFifo :: forall dom conf userType
           . ( HiddenClockResetEnable dom
             , KnownAxi4StreamConfig conf
             , NFDataX userType )
          => Circuit (Axi4Stream dom conf userType) (Axi4Stream dom conf PacketState)
crcFifo = Circuit circuitfunction
  where
    -- | Function that creates the signal to send to output of FIFO
    crcSetFlag :: (Signal dom (Maybe (Axi4StreamM2S conf userType))) -> Signal dom PacketState
    crcSetFlag axi4' = outPkt' where
        -- get current state
        inState' = register (crc_starting_state, Idle) outState'
        -- unbundle the signals
        (inCRC', inPkt') = unbundle inState'
        -- update the crc state
        crcUpdater :: CRCState -> Maybe (Axi4StreamM2S conf userType) -> CRCState
        crcUpdater crc axi4 = 
            case axi4 of
                Just pkt -> foldl (\c (a, b) -> if b then upd_crc_state c (pack a) else c) crc (zipWith (,) (_tdata pkt) (_tkeep pkt))
                Nothing  -> crc
        outCRC' = crcUpdater <$> inCRC' <*> axi4'
        -- update the packet state
        pktStateUpdater :: PacketState -> Maybe (Axi4StreamM2S conf userType) -> BitVector 32 -> PacketState
        pktStateUpdater inPkt axi4 outCRCValue = 
            case (maybe False _tlast axi4, outCRCValue == 0x2144DF1C) of
                (False, _)    -> inPkt
                (True, True)  -> PacketValid
                (True, False) -> PacketInvalid
        (outCRCValue', _) =  unbundle outCRC'
        outPkt' = pktStateUpdater <$> inPkt' <*> axi4' <*> outCRCValue'
        -- bundle output signal
        outState' = bundle (outCRC', outPkt')


    --circuitFunction :: (Signal (Maybe Axi4StreamM2S ...), Axi4StreamS2M) -> (Axi4StreamS2M, Signal (Maybe Axi4StreamM2S ...))
    circuitfunction (inpA, inpB) = (otpA, otpB) where
        flag = crcSetFlag inpA
        -- just assume this works :^)
        ack = (\b1 b2 -> Axi4StreamS2M {_tready = b1 && b2}) <$> (_tready <$> inpB) <*> ((/= Idle) <$> flag)
        fifoSignal :: ( Signal dom (Maybe (Axi4StreamM2S conf userType))
                      , Signal dom Axi4StreamS2M )
                      -> ( Signal dom Axi4StreamS2M
                         , Signal dom (Maybe (Axi4StreamM2S conf PacketState)) )
        fifoSignal = toSignals (signalFifo (SNat @1500) flag)
        (otpA, otpB) = fifoSignal (inpA, ack)


-- | Function that adds the fifo state signal to the outgoing traffic of the fifo
signalFifo :: forall (dom      :: Domain)
                     (conf     :: Axi4StreamConfig)
                     (userType :: Type)
                     (depth    :: Nat)
    . ( HiddenClockResetEnable dom
      , KnownAxi4StreamConfig conf
      , NFDataX userType
      , KnownNat depth
      , (1 <=? depth) ~ 'True)
   => SNat depth
   -> Signal dom PacketState
   -> Circuit (Axi4Stream dom conf userType) (Axi4Stream dom conf PacketState)
signalFifo depth state = addTuser state <| fifo Proxy Proxy depth

-- | Function that adds user data to the Axi4 Stream
addTuser :: forall (dom       :: Domain)
                   (conf      :: Axi4StreamConfig)
                   (userType1 :: Type)
                   (userType2 :: Type) 
    . ( HiddenClockResetEnable dom
      , KnownAxi4StreamConfig conf
      , NFDataX userType1, NFDataX userType2)
   => Signal dom userType2
   -> Circuit (Axi4Stream dom conf userType1) (Axi4Stream dom conf userType2)
addTuser tuser = Circuit $ circuitFunction where
    circuitFunction (inpA, inpB) = (otpA, otpB) where
        -- | simply forward ack
        otpA = inpB
        -- | forward by adding _tuser field
        otpB = func <$> tuser <*> inpA
        func tuser' inpA' = (\axi4 -> axi4 {_tuser = tuser'}) <$> inpA'