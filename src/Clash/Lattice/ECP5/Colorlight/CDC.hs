module Clash.Lattice.ECP5.Colorlight.CDC (readCDC, writeCDC) where

import Clash.Explicit.Prelude

type BufEl = (Vec 4 Byte, Index 4)
type Byte = BitVector 8

data RState = Accumulating BufEl | Empty
  deriving (Show, ShowX, Eq, Generic, NFDataX)
data WState = Idle | Writing BufEl
  deriving (Show, ShowX, Eq, Generic, NFDataX)

nextByte :: BufEl -> (Byte, Maybe BufEl)
nextByte (vec, 0) = (head vec, Nothing)
nextByte (vec, n) = (head vec, Just $ (rotateLeftS vec d1, n-1))

readCDC :: forall eth dom . (KnownDomain eth, KnownDomain dom)
    => Clock eth
    -> Clock dom
    -> Reset eth
    -> Reset dom
    -> Enable eth
    -> Enable dom
    -> Signal eth (Maybe Byte)
    -- | TODO: What exactly should be here as output?
    -- A BitVector 32 with an Index 4 to describe the amount of output bytes?
    -> Signal dom (Maybe BufEl)
readCDC eth_clk dom_clk eth_rst dom_rst eth_en dom_en inp = intoMaybe <$> hasRead <*> read
    where
        intoMaybe :: Bool -> a -> Maybe a
        intoMaybe True val = Just val
        intoMaybe False val = Nothing

        addrSize = d3
        buffer = asyncFIFOSynchronizer addrSize eth_clk dom_clk eth_rst dom_rst eth_en dom_en
        (read, empty_flag, full_flag) = buffer shouldRead stride
        shouldRead = register dom_clk dom_rst dom_en False $ not <$> empty_flag
        hasRead = register dom_clk dom_rst dom_en False $ shouldRead

        initialState = Empty
        stride :: Signal eth (Maybe BufEl)
        stride = mealy eth_clk eth_rst eth_en upd initialState inp
            where
                upd :: RState -> Maybe Byte -> (RState, Maybe BufEl)
                upd Empty Nothing = (Empty, Nothing)
                upd Empty (Just b) = (Accumulating (0:>0:>0:>b:>Nil, 0), Nothing)
                upd (Accumulating el) Nothing = (Empty, Just el)
                upd (Accumulating (vec,3)) (Just b) = (Accumulating (0:>0:>0:>b:>Nil, 0), Just (vec,3))
                upd (Accumulating (vec,n)) (Just b) = (Accumulating (replace 3 b $ rotateLeftS vec d1, n+1), Nothing)


writeCDC :: (KnownDomain eth, KnownDomain dom)
    => Clock eth
    -> Clock dom
    -> Reset eth
    -> Reset dom
    -> Enable eth
    -> Enable dom
    -- | TODO: What exactly should be here as input?
    -- A BitVector 32 with an Index 4 to describe the amount of output bytes?
    -> Signal dom (Maybe (Vec 4 Byte, Index 4))
    -- | 1. Output byte 2. Full flag
    -> (Signal eth (Maybe Byte), Signal dom Bool)
writeCDC eth_clk dom_clk eth_rst dom_rst eth_en dom_en inp = (output, full_flag)
    where
        addrSize = d3
        buffer = asyncFIFOSynchronizer addrSize dom_clk eth_clk dom_rst eth_rst dom_en eth_en
        (read, empty_flag, full_flag) = buffer shouldRead inp

        -- Stride converter
        initialState = Idle
        (output, shouldRead) = unbundle $ mealy eth_clk eth_rst eth_en upd initialState $ bundle (read, empty_flag)

        upd :: WState -> (BufEl, Bool) -> (WState, (Maybe Byte, Bool))
        upd Idle (_, True) = (Idle, (Nothing, False))
        upd Idle (el, False) = (nextState, (Just bv, True))
            where
                nextState = case nextEl of
                    Just e -> Writing e
                    Nothing -> Idle
                (bv, nextEl) = nextByte el
        upd (Writing el) (_, empty) = (nextState, (Just bv, False))
            where
                nextState = case nextEl of
                    Just e -> Writing e
                    Nothing -> Idle
                (bv, nextEl) = nextByte el
