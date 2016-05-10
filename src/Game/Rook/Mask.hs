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

import           Data.Word       (Word64)

import qualified Game.Rook.Coord as Coord

newtype Mask = Mask Word64 deriving (Eq)

instance Show Mask where
  show (Mask w) = "fromWord " ++ show w

fromWord :: Word64 -> Mask
fromWord = undefined

toWord :: Mask -> Word64
toWord = undefined

fromCoords :: [Coord.Coord] -> Mask
fromCoords = undefined

toCoords ::  Mask -> [Coord.Coord]
toCoords = undefined

empty :: Mask
empty = undefined

full :: Mask
full = undefined

intersection :: Mask -> Mask -> Mask
intersection = undefined

union :: Mask -> Mask -> Mask
union = undefined

remove :: Mask -> Mask -> Mask
remove = undefined

invert :: Mask -> Mask
invert = undefined
