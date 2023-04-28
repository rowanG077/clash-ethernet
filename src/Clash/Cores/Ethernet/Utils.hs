{-# LANGUAGE FlexibleContexts #-}

module Clash.Cores.Ethernet.Utils ( downconverter ) where

import Data.Maybe ( isNothing, isJust )
import Data.Tuple ( swap )
import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream


downconverter :: forall (dom       :: Domain)
                      (conf1     :: Axi4StreamConfig)
                      (conf2     :: Axi4StreamConfig)
                      (userType  :: Type)
    . ( HiddenClockResetEnable dom
      , KnownAxi4StreamConfig conf1
      , KnownAxi4StreamConfig conf2
      , DataWidth conf1 ~ 4, IdWidth conf1 ~ IdWidth conf2, DestWidth conf1 ~ DestWidth conf2
      , NFDataX userType, DataWidth conf2 ~ 1)
   => ( Signal dom (Maybe (Axi4StreamM2S conf1 userType))
      , Signal dom Axi4StreamS2M)
   -> ( Signal dom Axi4StreamS2M
      , Signal dom (Maybe (Axi4StreamM2S conf2 userType))
      )
downconverter ipt = swap $ unbundle $ mealy downconverter' (0, Nothing) $ bundle ipt

downconverter' :: forall (conf1     :: Axi4StreamConfig)
                       (conf2     :: Axi4StreamConfig)
                       (userType  :: Type)
    . ( KnownAxi4StreamConfig conf1
      , KnownAxi4StreamConfig conf2
      , DataWidth conf1 ~ 4, IdWidth conf1 ~ IdWidth conf2, DestWidth conf1 ~ DestWidth conf2
      , NFDataX userType, DataWidth conf2 ~ 1 ) 
   => ( Index 4, Maybe (Axi4StreamM2S conf1 userType) ) 
   -> ( (Maybe (Axi4StreamM2S conf1 userType))
        , Axi4StreamS2M )
   -> ( ( Index 4, Maybe(Axi4StreamM2S conf1 userType) )
        , ( (Maybe (Axi4StreamM2S conf2 userType))
          , Axi4StreamS2M) 
      )
downconverter' s@(idx, packet) (m2s, s2m)
    -- | if we are in the initial state we wait for incoming packet
    | idx == 0  = (nextState, (createM2S m2s, s2m))
    -- | we are ready and are not in the initial state -> send packet but dont accept any
    | otherwise = (nextState, (createM2S packet, notreadyS2M))
    where
        notreadyS2M = Axi4StreamS2M {_tready = False}
        createM2S p = creator <$> p
            where  creator p = Axi4StreamM2S {
                        _tdata = singleton . (!! idx) . _tdata $ p
                        , _tkeep = singleton . (!! idx) . _tkeep $ p
                        , _tstrb = singleton . (!! idx) . _tstrb $ p
                        , _tlast = isLast
                        , _tid   = _tid p
                        , _tdest = _tdest p
                        , _tuser = _tuser p
                   }
        isLast = maybe False _tlast packet && maybe False ((go 3) . _tkeep) packet
            where
                go :: Index 4 -> Vec 4 Bool -> Bool
                go x ls  = if x > idx
                            then (not . (!! x) $ ls) && go (x-1) ls
                            else True
        nextState
            | not $ _tready s2m = s -- if down stream is not ready we stay in same state
            | isLast = (0, packet)
            | idx == 0 && isJust m2s = (1, m2s)
            | idx == 0 && isNothing m2s = s
            | idx < 3 = (idx + 1, packet)
            | otherwise = (0, Nothing)


-- upconverter :: forall (dom        :: Domain)
--                         (conf1        :: Axi4StreamConfig)
--                         (conf2        :: Axi4StreamConfig)
--                         (userType    :: Type)
--     . (KnownDomain dom, KnownAxi4StreamConfig conf, NFDataX userType)
--     -> ( Axi4StreamM2S (conf1 :: Axi4StreamConfig) (userType :: Type)
--        , Axi4StreamS2M)
--     -> ( Axi4StreamM2S conf2 userType2
--        , Axi4StreamS2M)

-- upconverter = undefined
