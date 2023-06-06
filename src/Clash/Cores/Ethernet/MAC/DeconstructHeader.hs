module Clash.Cores.Ethernet.MAC.DeconstructHeader ( deconstructHeader ) where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream
import  Data.Maybe

import Clash.Cores.Ethernet.Stream
    ( EthernetHeader(..), SingleByteStream, SingleByteStreamFwd, TaggedSingleByteStream,
    TaggedSingleByteStreamFwd, mealyToCircuit )

data State = Buffering (Index 14) (Vec 14 (Unsigned 8)) | Passthrough EthernetHeader
  deriving (Show, Eq, Generic, NFDataX)

deconstructHeader :: HiddenClockResetEnable dom => Circuit (SingleByteStream dom) (TaggedSingleByteStream dom)
deconstructHeader = mealyToCircuit machineAsFunction initialState where
  initialState = Buffering 0 (replicate d14 0)
  ready = Axi4StreamS2M { _tready = True }
  machineAsFunction :: State -> (SingleByteStreamFwd, Axi4StreamS2M) -> (State, (Axi4StreamS2M, TaggedSingleByteStreamFwd))

  -- extracting the header
  machineAsFunction s@(Buffering _ _) (Nothing, _) = (s, (ready, Nothing))
  machineAsFunction s@(Buffering n header) (Just inp, _) = (nextState, (ready, Nothing))
    where
      newHeader = replace n (head $ (_tdata inp)) header
      nextState
<<<<<<< HEAD
        | _tlast inp  = initialState
            --  early appearence of _tlast
        | n < 13      =  Buffering (n+1) newHeader
        | otherwise   =  Passthrough ehHeader
=======
        | _tlast inp = initialState
            -- ^ early appearence of the _tlast
        | n == 13 =  Passthrough ehHeader
        | n < 13 =  Buffering (n+1) newHeader
>>>>>>> c6da99553f2ac5be7b10dc8309af13cf183ae81c
        where
          (dest, src, eth) = unpack $ pack newHeader
          ehHeader = EthernetHeader { destinationMAC = dest, sourceMAC = src, etherType = eth}

<<<<<<< HEAD

   -- Passthrough, just receives the rest of the packet  
=======
>>>>>>> c6da99553f2ac5be7b10dc8309af13cf183ae81c
  machineAsFunction s@(Passthrough _ ) (Nothing, recvACK) = (s, (recvACK, Nothing))
  machineAsFunction s@(Passthrough header) (Just inp, recvACK) = (s, (recvACK, out))
    where
      nextState
        | not $ _tready recvACK = s
        | _tlast inp = initialState
<<<<<<< HEAD
           -- ^ early appearence of the -tlast
        | otherwise = Passthrough header
      out = Just Axi4StreamM2S{  _tdata  =_tdata inp
                               , _tkeep = _tkeep inp
                               , _tstrb = _tstrb inp
                               , _tlast = _tlast inp
                               , _tuser = header
                               , _tid = _tid inp
                               , _tdest = _tdest inp
                              }
=======
        | otherwise = Passthrough header
      out = Just Axi4StreamM2S{  _tdata  =_tdata inp
                              , _tkeep = _tkeep inp
                              , _tstrb = _tstrb inp
                              , _tlast = _tlast inp
                              , _tuser = header
                              , _tid = _tid inp
                              , _tdest = _tdest inp
                              }
>>>>>>> c6da99553f2ac5be7b10dc8309af13cf183ae81c
