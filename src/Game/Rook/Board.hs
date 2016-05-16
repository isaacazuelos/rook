-- |
-- Module      : Game.Rook.Board
-- Description : Board Representation
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- This is a raw board, with no other game state information.

module Game.Rook.Board
    ( Board
    , material
    , empty
    , starting
    , get
    , set
    )
  where

import           Game.Rook.Piece

import qualified Game.Rook.Mask  as Mask

data Board = Board deriving (Show, Eq)

material :: Board -> Colour -> Mask.Mask
material = undefined

empty :: Board
empty = undefined

starting :: Board
starting = undefined

get :: Board -> Colour -> Piece -> Mask.Mask
get = undefined

set :: Board -> Colour -> Piece -> Mask.Mask -> Board
set = undefined
