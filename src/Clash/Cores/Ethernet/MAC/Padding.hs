module Clash.Cores.Ethernet.MAC.Padding ( payloadPadder ) where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream

import Clash.Cores.Ethernet.Stream

-- NOTE: For `Index 47` we have `maxBound == 46` which is the minimum payload length
data State = Counting (Index 47) | AddPadding (Index 47) EthernetHeader
  deriving (Show, Eq, Generic, NFDataX)

-- | Ensures the minimum package size is reached by padding the payload with null bytes if necessary
--
-- Should only receive the payload as input stream!
payloadPadder :: HiddenClockResetEnable dom => Circuit (TaggedStream dom) (TaggedStream dom)
payloadPadder = mealyToCircuit machineAsFunction initialState where
  notReady = Axi4StreamS2M { _tready = False }
  initialState = Counting 0

  machineAsFunction :: State -> (TaggedStreamFwd, Axi4StreamS2M) -> (State, (Axi4StreamS2M, TaggedStreamFwd))
  machineAsFunction (AddPadding n user) (_, recvACK) = (newState, (notReady, Just nullByte))
    where
      finished = newCount >= maxBound
      newCount = boundedAdd n 4
      newState
        -- No transfer happening this clock cycle
        | not $ _tready recvACK = AddPadding n user
        -- Successfully padded until the minimum length, we can stop now
        | finished = initialState
        -- Keep padding null bytes
        | otherwise = AddPadding newCount user
      nullByte = Axi4StreamM2S
                   { _tdata = replicate d4 0
                   , _tkeep = map (\x -> n <= maxBound - x) $ generate d4 (+1) 0
                   , _tstrb = replicate d4 False
                   , _tlast = finished
                   , _tuser = user
                   , _tid = 0
                   , _tdest = 0
                   }
  machineAsFunction (Counting n) (Nothing, recvACK) = (Counting n, (recvACK, Nothing))
  machineAsFunction (Counting n) (Just inp, recvACK) = (newState, (recvACK, output))
    where
      finished = newCount >= maxBound && _tlast inp
      newCount = foldl boundedAdd n $ map (\b -> if b then 1 else 0) $ _tkeep inp
      output = Just Axi4StreamM2S
                 { _tdata = _tdata inp
                 , _tkeep = _tkeep inp
                 , _tstrb = _tstrb inp
                 , _tlast = finished
                 , _tuser = _tuser inp
                 , _tid = _tid inp
                 , _tdest = _tdest inp
                 }
      newState
        -- No transfer happening this clock cycle
        | not $ _tready recvACK = Counting n
        -- Packet ends, we need to add a padding
        | newCount < maxBound && _tlast inp = AddPadding newCount (_tuser inp)
        -- The packet ends here and is long enough, get ready for the next one.
        | finished = initialState
        -- Still too short, keep counting
        | newCount < maxBound = Counting newCount
        -- We've become long enough
        | otherwise = Counting n
