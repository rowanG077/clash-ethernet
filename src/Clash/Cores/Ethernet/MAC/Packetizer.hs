module Clash.Cores.Ethernet.MAC.Packetizer ( macPacketizer ) where

import Clash.Prelude
import Data.Maybe ( isNothing )
import Protocols
import Protocols.Axi4.Stream

import Clash.Cores.Ethernet.Stream ( FourByteStream, FourByteStreamFwd, mealyToCircuit )

-- | id of stream we're routing
data State n = Idle (Index n) | Streaming (Index n)
  deriving (Show, Eq, Generic, NFDataX)

-- | Forwards packets from all inputs to output in a `roundRobin Skip`-like fashion.
--
-- Can potentially block for a while but is guaranteed to always eventually route an input to the output.
macPacketizer :: (HiddenClockResetEnable dom, KnownNat n, (1 <=? n) ~ 'True) => Circuit (Vec n (Axi4Stream dom conf user)) (Axi4Stream dom conf user)
macPacketizer = Circuit $ circuitFunction where
  notReady = Axi4StreamS2M { _tready = False }
  initialState = Idle 0
  circuitFunction = (\(a,b) -> (unbundle a, b))
                  . unbundle
                  . mealy machineAsFunction initialState
                  . bundle
                  . (\(a,b) -> (bundle a, b))

  machineAsFunction :: (KnownNat n, (1 <=? n) ~ 'True) => State n -> (Vec n (Maybe (Axi4StreamM2S conf user)), Axi4StreamS2M) -> (State n, (Vec n Axi4StreamS2M, Maybe (Axi4StreamM2S conf user)))

  -- We're not routing anything, is this stream sending?
  machineAsFunction (Idle ind) (vec, recvACK) = (nextState, (replace ind recvACK $ repeat notReady, vec !! ind)) where
    nextState
      | isNothing (vec !! ind) = Idle $ satSucc SatWrap ind
      | otherwise = Streaming ind

  -- We're routing this stream, is the _tlast set?
  machineAsFunction (Streaming ind) (vec, recvACK) = (nextState (vec !! ind), (replace ind recvACK $ repeat notReady, vec !! ind)) where
    nextState Nothing = Streaming ind
    nextState (Just input)
      | _tlast input && _tready recvACK = Idle (satSucc SatWrap ind)
      | otherwise = Streaming ind
