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
      newState
        -- No transfer happening this clock cycle
        | not $ _tready recvACK = AddPadding n user
        -- Successfully padded until the minimum length
        | n + 1 >= 46 = initialState
        -- Keep padding null bytes
        | n + 1 < 46 = AddPadding (n+1) user
      nullByte = Axi4StreamM2S
                   { _tdata = singleton 0
                   , _tkeep = singleton True
                   , _tstrb = singleton False
                   , _tlast = n+1 >= 46
                   , _tuser = user
                   , _tid = 0
                   , _tdest = 0
                   }
  machineAsFunction (TooShort n) (Nothing, recvACK) = (TooShort n, (recvACK, Nothing))
  machineAsFunction (TooShort n) (Just inp, recvACK) = (newState, (recvACK, output))
    where
      newCount = if head $ _tkeep inp then n+1 else n
      output
        -- Too short for this to be the last packet, we still need to add some padding
        | newCount < 46 && _tlast inp = Just Axi4StreamM2S
                                           { _tdata = _tdata inp
                                           , _tkeep = _tkeep inp
                                           , _tstrb = _tstrb inp
                                           , _tlast = False
                                           , _tuser = _tuser inp
                                           , _tid = _tid inp
                                           , _tdest = _tdest inp
                                           }
        | otherwise = Just inp
      newState
        -- No transfer happening this clock cycle
        | not $ _tready recvACK = TooShort n
        -- Packet ends, we need to add a padding
        | newCount < 46 && _tlast inp = AddPadding newCount (_tuser inp)
        -- Still too short, keep counting
        | newCount < 46 = TooShort newCount
        -- Handle edge case!
        | newCount >= 46 && _tlast inp = initialState
        -- We've become long enough
        | otherwise = LongEnough
