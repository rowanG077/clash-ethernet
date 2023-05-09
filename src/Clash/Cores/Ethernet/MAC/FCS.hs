module Clash.Cores.Ethernet.MAC.FCS ( fcsAppender ) where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream

import Clash.Cores.Ethernet.Stream ( mealyToCircuit, SingleByteStream, SingleByteStreamFwd )
import Clash.Cores.Ethernet.CRC ( CRCState, finish_crc_state, crc_starting_state, upd_crc_state )

data FCSState = Accumulating CRCState | Appending (Index 4) (Vec 4 Byte)
  deriving (Show, Eq, Generic, NFDataX)

type Byte = BitVector 8

-- | Calculates and appends the FCS (checksum) to the frame
fcsAppender :: HiddenClockResetEnable dom => Circuit (SingleByteStream dom) (SingleByteStream dom)
fcsAppender = mealyToCircuit machineAsFunction starting_state where
  starting_state = Accumulating crc_starting_state
  reverseEndianness :: forall n . KnownNat n => BitVector n -> BitVector n
  reverseEndianness bv = pack $ reverse vec
      where
        vec :: Vec n Bit
        vec = unpack bv

  machineAsFunction :: FCSState -> (SingleByteStreamFwd, Axi4StreamS2M) -> (FCSState, (Axi4StreamS2M, SingleByteStreamFwd))
  machineAsFunction (Accumulating s) (Just inp, recvACK) = (newState, (recvACK, out))
    where
      -- Always set _tlast to False
      out = Just Axi4StreamM2S { _tdata = _tdata inp
                               , _tkeep = _tkeep inp
                               , _tstrb = _tstrb inp
                               , _tlast = False
                               , _tuser = _tuser inp
                               , _tid = _tid inp
                               , _tdest = _tdest inp
                               }
      -- Update the CRC state with an extra byte of input
      newCRC = upd_crc_state s (reverseEndianness $ pack $ _tdata inp)
      newState
        -- No transmit taking place this cycle, keep in same state
        | not (_tready recvACK) = Accumulating s
        -- No transmit taking place this cycle, keep in same state
        | _tlast inp = Appending 0 (unpack $ finish_crc_state newCRC)
        | otherwise = Accumulating newCRC
  machineAsFunction (Appending ind bv) (_, recvACK) = (newState, (notReady, out))
    where
      notReady = Axi4StreamS2M { _tready = False }
      -- NOTE: Take care with ordering!
      out = Just Axi4StreamM2S { _tdata = singleton $ unpack $ reverseEndianness $ pack $ bv !! ind
                               , _tkeep = singleton True
                               , _tstrb = singleton False
                               , _tlast = ind == 3
                               , _tuser = ()
                               , _tid = 0
                               , _tdest = 0
                               }
      newState | not $ _tready recvACK = Appending ind bv
               | ind == 3 = starting_state
               | otherwise = Appending (ind+1) bv
  machineAsFunction s (Nothing, recvACK) = (s, (recvACK, Nothing))
