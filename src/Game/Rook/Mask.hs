-- |
-- Module      : Game.Rook.Mask
-- Description : A board mask
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- A mask is a  representation for what can be thought of as a set of `Coord`s.

module Game.Rook.Mask
    (
      Mask
    , empty
    , full
    -- * Contruction
    , fromWord
    , toWord
    , fromCoords
    , toCoords
    -- * Operations
    , intersection
    , union
    , remove
    , invert
    )
  where

import           Data.Bits       (complement, zeroBits, (.&.), (.|.))
import           Data.Word       (Word64)

import qualified Game.Rook.Coord as Coord

newtype Mask = Mask Word64 deriving (Eq)

instance Show Mask where
  show m = "fromCoords " ++ show (toCoords m)

-- | Create a `Mask` from a `Word64`, using the numbering scheme outlined from
-- `Coord.toWord`
fromWord :: Word64 -> Mask
fromWord = Mask

-- | Turns a `Mask` into a `Word64`, using the numbering shceme outlined fromWord
-- `Coord.toWord`
toWord :: Mask -> Word64
toWord (Mask m) = m

-- | Create a `Mask` where the `Coord.Coord`s in the list are set.
--
-- @
-- toCoords . fromCoords === id
-- fromCoords . toCoords === id
-- @
fromCoords :: [Coord.Coord] -> Mask
fromCoords cs = Mask $ foldr (\ c w -> Coord.toWord c .|. w) zeroBits cs

-- | List the `Coord.Coord`s in the `Mask`.
--
-- @
-- fromCoords . toCoords === id
-- toCoords . fromCoords === id
-- @
toCoords ::  Mask -> [Coord.Coord]
toCoords m = filter (`on` m) Coord.allCoords
  where on c (Mask w) = (Coord.toWord c .|. w) == w

-- | A `Mask` with no `Coord.Coord`s set.
empty :: Mask
empty = Mask 0

-- | A `Mask` with every `Coord.Coord`s set.
full :: Mask
full = Mask (complement 0)

-- | The same as set intersection, but using `Mask`s.
intersection :: Mask -> Mask -> Mask
intersection (Mask a) (Mask b) = Mask (a .&. b)

-- | The same as set union, but using `Mask`s.
union :: Mask -> Mask -> Mask
union (Mask a) (Mask b) = Mask (a .|. b)

-- | Remove the second `Mask` from the first.
--
-- > 1 0 1          1 1 0   0 0 1
-- > 0 1 0 `remove` 1 1 0 = 0 0 0
-- > 1 0 1          0 0 0   1 0 1
remove :: Mask -> Mask -> Mask
remove (Mask a) (Mask b) = Mask (a .&. complement b)

-- | A mask where all the set `Coord.Coord`s are unset, and all the unset ones
-- are set.
--
-- >        1 0 1   0 1 0
-- > invert 0 1 0 = 1 0 1
-- >        1 0 1   0 1 0
invert :: Mask -> Mask
invert (Mask m) = Mask (complement m)
