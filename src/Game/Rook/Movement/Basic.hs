-- |
-- Module      : Game.Rook.Piece
-- Description : Rules for basic piece movement
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- Basic piece movements, as used by kings, queens, bishops and rooks.
--
-- There are two basic types of movement, stepping can casting.
--
-- * Stepping is moving a piece a square in a direction. `Nothing` is used to
--   indicate that the coord cannot step in that direction. This happens either
--   becasue the piece would step off the board or becuase the piece would
--   capture a piece of the same colour.
--
-- * Casting creates a list of steps, until further stepping would be invalid.

module Game.Rook.Movement.Basic
    ( Direction ( .. )
    , Movement ( Movement )
    , from
    , to
    , capture
    , apply
    , step
    , cast
    )
  where

import Game.Rook.Board
import Game.Rook.Coord
import Game.Rook.Piece

-- | Pieces can be moved in each of the cardinal or intercardinal directions.
-- We're saying that coordinate `a1` is the southwest corner and `h8` is the
-- northeast corner.
data Direction
    = N | NE | E | SE | S | SW | W | NW 
  deriving (Show, Eq)

-- | A movement describes moving a piece from one place to another.
data Movement
    = Movement
      { from :: Coord
      , to :: Coord }
  deriving (Show, Eq)

-- | Is a piece captured by the movement?
capture :: Board -> Movement -> Maybe Piece
capture = undefined

-- | Give the board after a movement, regardless of movement validity. Apply
-- just moves the piece at the `from` coord to the `to` coord, and sets the
-- `from` to `mempty`.
apply :: Board -> Movement -> Board
apply = undefined

-- | Step the piece at a coord in a particular direction. 
step :: Board -> Coord -> Direction -> Maybe Movement
step = undefined

-- | Cast out the piece at a coord in a particular direction. 
cast :: Board -> Coord -> Direction -> [Movement]
cast = undefined

