-- |
-- Module      : Game.Rook.Coord
-- Description : Chess Coordinates
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A coordinate refers to a single location on the board.
--
-- Since there are a finite number of them, they're simply enumerated.

module Game.Rook.Coord
    ( -- * Coord
      Coord
    , toWord
    , allCoords
      -- * Coordinates
    , a1, a2, a3, a4, a5, a6, a7, a8
    , b1, b2, b3, b4, b5, b6, b7, b8
    , c1, c2, c3, c4, c5, c6, c7, c8
    , d1, d2, d3, d4, d5, d6, d7, d8
    , e1, e2, e3, e4, e5, e6, e7, e8
    , f1, f2, f3, f4, f5, f6, f7, f8
    , g1, g2, g3, g4, g5, g6, g7, g8
    , h1, h2, h3, h4, h5, h6, h7, h8
    )
  where

import           Data.Bits  (bit)
import           Data.Maybe (fromMaybe)
import           Data.Word  (Word64)


-- | This is used to track a single square.
newtype Coord = Coord Word64 deriving (Eq)

instance Show Coord where
  show c = fromMaybe "<invalid coord>" . lookup c $ zip allCoords allNames
    where
      allNames =
        [ "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8"
        , "b1", "b2", "b3", "b4", "b5", "b6", "b7", "b8"
        , "c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"
        , "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8"
        , "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8"
        , "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8"
        , "g1", "g2", "g3", "g4", "g5", "g6", "g7", "g8"
        , "h1", "h2", "h3", "h4", "h5", "h6", "h7", "h8"
        ]

-- | The squares are numbered according to the board below. The coordinate is
-- represted by the word where a single bit is set, according to the diagram
-- below.
--
-- >    8 56 57 58 59 60 61 62 63
-- >    7 48 49 50 51 52 53 54 55
-- >    6 40 41 42 43 44 45 46 47
-- >    5 32 33 34 35 36 37 38 39
-- >    4 24 25 26 27 28 29 30 31
-- >    3 16 17 18 19 20 21 22 23
-- >    2 08 09 10 11 12 13 14 15
-- >    1 00 01 02 03 04 05 06 07
-- >      a  b  c  d  e  f  g  h
toWord :: Coord -> Word64
toWord (Coord w) = w

-- | All coordinates, enumerated lexicographically.
allCoords :: [Coord]
allCoords = [ a1, a2, a3, a4, a5, a6, a7, a8
            , b1, b2, b3, b4, b5, b6, b7, b8
            , c1, c2, c3, c4, c5, c6, c7, c8
            , d1, d2, d3, d4, d5, d6, d7, d8
            , e1, e2, e3, e4, e5, e6, e7, e8
            , f1, f2, f3, f4, f5, f6, f7, f8
            , g1, g2, g3, g4, g5, g6, g7, g8
            , h1, h2, h3, h4, h5, h6, h7, h8
            ]

-- This is a hack to get haddock to shut up about the lack of documentation.
-- They're chess coordinate names. They don't need it.

-- | /  /
a1, a2, a3, a4, a5, a6, a7, a8 :: Coord
a1 = Coord (bit 0)
a2 = Coord (bit 8)
a3 = Coord (bit 16)
a4 = Coord (bit 24)
a5 = Coord (bit 32)
a6 = Coord (bit 40)
a7 = Coord (bit 48)
a8 = Coord (bit 56)

-- | /  /
b1, b2, b3, b4, b5, b6, b7, b8 :: Coord
b1 = Coord (bit 1)
b2 = Coord (bit 9)
b3 = Coord (bit 17)
b4 = Coord (bit 25)
b5 = Coord (bit 33)
b6 = Coord (bit 41)
b7 = Coord (bit 49)
b8 = Coord (bit 57)

-- | /  /
c1, c2, c3, c4, c5, c6, c7, c8 :: Coord
c1 = Coord (bit 2)
c2 = Coord (bit 10)
c3 = Coord (bit 18)
c4 = Coord (bit 26)
c5 = Coord (bit 34)
c6 = Coord (bit 42)
c7 = Coord (bit 50)
c8 = Coord (bit 58)

-- | /  /
d1, d2, d3, d4, d5, d6, d7, d8 :: Coord
d1 = Coord (bit 3)
d2 = Coord (bit 11)
d3 = Coord (bit 19)
d4 = Coord (bit 27)
d5 = Coord (bit 35)
d6 = Coord (bit 43)
d7 = Coord (bit 51)
d8 = Coord (bit 59)

-- | /  /
e1, e2, e3, e4, e5, e6, e7, e8 :: Coord
e1 = Coord (bit 4)
e2 = Coord (bit 12)
e3 = Coord (bit 20)
e4 = Coord (bit 28)
e5 = Coord (bit 36)
e6 = Coord (bit 44)
e7 = Coord (bit 52)
e8 = Coord (bit 60)

-- | /  /
f1, f2, f3, f4, f5, f6, f7, f8 :: Coord
f1 = Coord (bit 5)
f2 = Coord (bit 13)
f3 = Coord (bit 21)
f4 = Coord (bit 29)
f5 = Coord (bit 37)
f6 = Coord (bit 45)
f7 = Coord (bit 53)
f8 = Coord (bit 61)

-- | /  /
g1, g2, g3, g4, g5, g6, g7, g8 :: Coord
g1 = Coord (bit 6)
g2 = Coord (bit 14)
g3 = Coord (bit 22)
g4 = Coord (bit 30)
g5 = Coord (bit 38)
g6 = Coord (bit 46)
g7 = Coord (bit 54)
g8 = Coord (bit 62)

-- | /  /
h1, h2, h3, h4, h5, h6, h7, h8 :: Coord
h1 = Coord (bit 7)
h2 = Coord (bit 15)
h3 = Coord (bit 23)
h4 = Coord (bit 31)
h5 = Coord (bit 39)
h6 = Coord (bit 47)
h7 = Coord (bit 55)
h8 = Coord (bit 63)
