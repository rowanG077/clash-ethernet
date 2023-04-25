{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}


module Clash.Cores.Ethernet.CDC ( circuitCDC, circuitTmp ) where

import Clash.Prelude
import Clash.Explicit.Synchronizer ( asyncFIFOSynchronizer )
import Protocols.Axi4.Stream
import Protocols

type BufEl = (Vec 4 Byte, Index 4)
type Byte = BitVector 8

data RState = Accumulating BufEl | Empty
  deriving (Show, ShowX, Eq, Generic, NFDataX)
data WState = Idle | Writing BufEl
  deriving (Show, ShowX, Eq, Generic, NFDataX)

nextByte :: BufEl -> (Byte, Maybe BufEl)
nextByte (vec, 0) = (head vec, Nothing)
nextByte (vec, n) = (head vec, Just $ (rotateLeftS vec d1, n-1))


circuitTmp :: forall (wdom        :: Domain)
                     (rdom        :: Domain)
                     (conf        :: Axi4StreamConfig)
                     (userType    :: Type)
   . (KnownDomain wdom, KnownDomain rdom, KnownAxi4StreamConfig conf, NFDataX userType)
  => Clock wdom
  -> Clock rdom
  -> Reset wdom
  -> Reset rdom
  -> Enable wdom
  -> Enable rdom
  -> Circuit (Axi4Stream wdom conf userType) (Axi4Stream rdom conf userType)
circuitTmp a b c d e f = Circuit $ circuitCDC a b c d e f

-- CDC as circuit
circuitCDC :: forall (wdom        :: Domain)
                     (rdom        :: Domain)
                     (conf        :: Axi4StreamConfig)
                     (userType    :: Type)
   . (KnownDomain wdom, KnownDomain rdom, KnownAxi4StreamConfig conf, NFDataX userType)
  => Clock wdom
  -> Clock rdom
  -> Reset wdom
  -> Reset rdom
  -> Enable wdom
  -> Enable rdom
  -> (Signal wdom (Maybe (Axi4StreamM2S conf userType)),
      Signal rdom (Axi4StreamS2M))
  -> ( Signal wdom (Axi4StreamS2M)
     , Signal rdom (Maybe (Axi4StreamM2S conf userType))
     )
circuitCDC wClk rClk wRst rRst wEn rEn ipt = (otp_s2m', otp_m2s')
    where
        (m2s, s2m) = ipt
        (m2s', empty, full) = asyncFIFOSynchronizer d3 wClk rClk wRst rRst wEn rEn readReq m2s
        otp_m2s' = mux empty (pure Nothing) (Just <$> m2s')
        otp_s2m' = bundle $ (\x -> Axi4StreamS2M { _tready = not x }) <$> full
        readReq :: Signal rdom Bool
        readReq = _tready <$> s2m
