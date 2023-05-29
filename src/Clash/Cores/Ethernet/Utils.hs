{-# LANGUAGE FlexibleContexts #-}

module Clash.Cores.Ethernet.Utils ( downconverter, upconverter ) where

import Data.Maybe ( isNothing, isJust )
import Data.Tuple ( swap )
import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream
import Clash.Cores.Ethernet.Stream ( mealyToCircuit )


downconverter :: forall (dom       :: Domain)
                        (conf1     :: Axi4StreamConfig)
                        (conf2     :: Axi4StreamConfig)
                        (userType  :: Type)
    . ( HiddenClockResetEnable dom
      , KnownAxi4StreamConfig conf1
      , KnownAxi4StreamConfig conf2
      , DataWidth conf1 ~ 4, IdWidth conf1 ~ IdWidth conf2, DestWidth conf1 ~ DestWidth conf2
      , NFDataX userType, DataWidth conf2 ~ 1)
   => Circuit (Axi4Stream dom conf1 userType) (Axi4Stream dom conf2 userType)
downconverter = mealyToCircuit downconverter' (0, Nothing)

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
   -> ( ( Index 4, Maybe (Axi4StreamM2S conf1 userType) )
        , ( Axi4StreamS2M, Maybe (Axi4StreamM2S conf2 userType))
      )
downconverter' s@(idx, packet) (m2s, s2m)
    -- | if we are in the initial state we wait for incoming packet
    | idx == 0  = (nextState, (s2m, createM2S m2s))
    -- | we are ready and are not in the initial state -> send packet but dont accept any
    | otherwise = (nextState, (notreadyS2M, createM2S packet))
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
            | not $ _tready s2m         = s -- if down stream is not ready we stay in same state
            | isLast                    = (0, packet)
            | idx == 0 && isJust m2s    = (1, m2s)
            | idx == 0 && isNothing m2s = s
            | idx < 3                   = (idx + 1, packet)
            | otherwise                 = (0, Nothing)



upconverter :: forall (dom        :: Domain)
                      (conf1      :: Axi4StreamConfig)
                      (conf2      :: Axi4StreamConfig)
                      (userType   :: Type)
    . ( HiddenClockResetEnable dom
      , KnownAxi4StreamConfig conf1, KnownAxi4StreamConfig conf2
      , NFDataX userType, DataWidth conf1 ~ 1, IdWidth conf1 ~ IdWidth conf2
      , DestWidth conf1 ~ DestWidth conf2, DataWidth conf2 ~ 4 )
    => Circuit (Axi4Stream dom conf1 userType) (Axi4Stream dom conf2 userType)
upconverter = mealyToCircuit upconverter' (0, Nothing)


upconverter' ::  forall (conf1      :: Axi4StreamConfig)
                        (conf2      :: Axi4StreamConfig)
                        (userType   :: Type)
    . ( KnownAxi4StreamConfig conf1
      , KnownAxi4StreamConfig conf2
      , NFDataX userType
      , DataWidth conf1 ~ 1, IdWidth conf1 ~ IdWidth conf2, DestWidth conf1 ~ DestWidth conf2
      , NFDataX userType, DataWidth conf2 ~ 4 )
    => ( Index 4, Maybe (Axi4StreamM2S conf2 userType) )
    -> ( ( Maybe (Axi4StreamM2S conf1 userType) )
       , Axi4StreamS2M )
    -> ( ( Index 4, Maybe (Axi4StreamM2S conf2 userType) )
       , ( Axi4StreamS2M, Maybe (Axi4StreamM2S conf2 userType))
       )
upconverter' s@(idx, packet) (m2s, s2m)
  = case (idx, m2s, _tready s2m) of
      (3, _, False)      -> (s, (notReady, packet))
      (_, _, False)      -> (s, (notReady, Nothing))
      (3, Nothing, True) -> (s, (ready, packet))
      (_, Nothing, True) -> (s, (ready, Nothing))
      (3, Just b, True)  -> ((0, Nothing), (ready, newPacket b))
      (i, Just b, True)  ->
        if _tlast b
          then ((0, Nothing), (ready, newPacket b))
          else ((i+1, newPacket b), (ready, Nothing))
  where
    ready = Axi4StreamS2M { _tready = True }
    notReady = Axi4StreamS2M { _tready = False }

    newPacket b =
      case packet of
        Nothing -> Just $ createPacket b initPacket
        Just p  -> Just $ createPacket b p
    createPacket b p = p
      { _tdata = replace idx (head . _tdata $ b) (_tdata p)
      , _tkeep = replace idx (head . _tkeep $ b) (_tkeep p)
      , _tstrb = replace idx (head . _tstrb $ b) (_tstrb p)
      , _tlast = _tlast b
      , _tid   = _tid b
      , _tdest = _tdest b
      , _tuser = _tuser b
      }
    initPacket = Axi4StreamM2S
      { _tdata = replicate d4 0
      , _tkeep = repeat False
      , _tstrb = repeat True
      , _tlast = False
      , _tid   = 0
      , _tdest = 0
      }
