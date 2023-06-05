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
  machineAsFunction s@(Buffering _ _) (Nothing, _) = (s, (ready, Nothing))
  machineAsFunction s@(Passthrough _ ) (Nothing, _) = (s, (ready, Nothing))
  machineAsFunction s@(Buffering n header) (Just inp, recvACK) = (nextState, (recvACK, out))
   where
      nextState
       
        | n < 14 =  Buffering (n+1) (replace n (head $ (_tdata inp)) header)
        | _tlast inp  = Buffering 0 (replicate d14 0)
            -- ^ early appearence of the _tlast 
        | n == 14 &&  _tready recvACK =  Passthrough   ehHeader
        where
          dest = take d6 header
          src  = take d6 (drop d6 header)
          eth  = unpack $ pack $ drop d12 header
          ehHeader = EthernetHeader { destinationMAC = dest, sourceMAC = src, etherType = eth}
      
      
      out=Nothing
      
  machineAsFunction s@(Passthrough header) (Just inp, recvACK) 
        |_tlast inp   = (Buffering 0 (replicate d14 0), (recvACK, Nothing))
        |otherwise           = (Passthrough header, (recvACK, out))
              where 
                out = Just Axi4StreamM2S{  _tdata  =_tdata inp
                                         , _tkeep = _tkeep inp
                                         , _tstrb = _tstrb inp
                                         , _tlast = _tlast inp
                                         ,_tuser = EthernetHeader { destinationMAC = destinationMAC header 
                                                                     , sourceMAC = sourceMAC header 
                                                                     , etherType = etherType header 
                                                                  }
                                         , _tid = _tid inp
                                         , _tdest = _tdest inp
                                       }
     
