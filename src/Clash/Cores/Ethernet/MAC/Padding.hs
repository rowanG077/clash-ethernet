module Clash.Cores.Ethernet.MAC.Padding ( payloadPadder ) where

import Clash.Prelude
import Protocols
import Protocols.Axi4.Stream

import Clash.Cores.Ethernet.Stream
    ( EthernetHeader, TaggedSingleByteStream, TaggedSingleByteStreamFwd, mealyToCircuit )

data State = LongEnough | TooShort (Index 47) | AddPadding (Index 47) EthernetHeader
  deriving (Show, Eq, Generic, NFDataX)

-- | Ensures the minimum package size is reached by padding the payload with null bytes if necessary
--
-- Should only receive the payload as input stream!
payloadPadder :: HiddenClockResetEnable dom => Circuit (TaggedSingleByteStream dom) (TaggedSingleByteStream dom)
payloadPadder = mealyToCircuit machineAsFunction initialState where
  notReady = Axi4StreamS2M { _tready = False }
  initialState = TooShort 0

  machineAsFunction :: State -> (TaggedSingleByteStreamFwd, Axi4StreamS2M) -> (State, (Axi4StreamS2M, TaggedSingleByteStreamFwd))
  machineAsFunction LongEnough (inp, recvACK) = (newState, (recvACK, inp))
    where
      newState = case _tlast <$> inp of
        -- This is the end of the payload, reset state
        Just True -> initialState
        -- We're already long enough
        _ -> LongEnough
  machineAsFunction (AddPadding n user) (_, recvACK) = (newState, (notReady, Just nullByte))
    where
      finished = n >= 45
      newState
        -- No transfer happening this clock cycle
        | not $ _tready recvACK = AddPadding n user
        -- Successfully padded until the minimum length, we can stop now
        | finished = initialState
        -- Keep padding null bytes
        | otherwise = AddPadding (n+1) user
      nullByte = Axi4StreamM2S
                   { _tdata = singleton 0
                   , _tkeep = singleton True
                   , _tstrb = singleton True
                   , _tlast = finished
                   , _tuser = user
                   , _tid = 0
                   , _tdest = 0
                   }
  machineAsFunction (TooShort n) (Nothing, recvACK) = (TooShort n, (recvACK, Nothing))
  machineAsFunction (TooShort n) (Just inp, recvACK) = (newState, (recvACK, output))
    where
      newCount = if head $ _tkeep inp then n+1 else n
      output = Just Axi4StreamM2S
                 { _tdata = _tdata inp
                 , _tkeep = _tkeep inp
                 , _tstrb = _tstrb inp
                 , _tlast = False
                 , _tuser = _tuser inp
                 , _tid = _tid inp
                 , _tdest = _tdest inp
                 }
      newState
        -- No transfer happening this clock cycle
        | not $ _tready recvACK = TooShort n
        -- Packet ends, we need to add a padding
        | newCount < 46 && _tlast inp = AddPadding newCount (_tuser inp)
        -- With next byte it will be long enough.
        | newCount == 45 = LongEnough
        -- Still too short, keep counting
        | newCount < 46 = TooShort newCount
        -- We've become long enough
        | otherwise = LongEnough
