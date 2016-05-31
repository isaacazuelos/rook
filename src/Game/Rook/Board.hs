-- |
-- Module      : Game.Rook.Board
-- Description : Board Representation
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- This is a chess board, with no other game state information. It contains no
-- other information about the state of a normal chess game, like the en-passant
-- state, or the availability of castling, or the status of the 50 move rule.
-- For working with that information, use a `Game.Rook.Game.Game`.
--
-- Our Board is a collection of `Mask`s, one for each type of piece and colour.
-- Each of these masks is kept strict.

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
import           Game.Rook.Coord

import qualified Game.Rook.Mask  as Mask

-- | A chess board. This is just the board itself, with no information about the
-- state of a game.
data Board =
  Board
    { king   :: !Mask.Mask
    , queen  :: !Mask.Mask
    , rook   :: !Mask.Mask
    , knight :: !Mask.Mask
    , bishop :: !Mask.Mask
    , pawn   :: !Mask.Mask
    , white  :: !Mask.Mask
    , black  :: !Mask.Mask
    }
  deriving (Show, Eq)

-- | All of the locations where a colour has pieces.
material :: Board -> Colour -> Mask.Mask
material b White = white b
material b Black = black b

-- | A board with no pieces on it.
empty :: Board
empty = let e = Mask.empty in Board e e e e e e e e

-- | A board in the usual starting position.
starting :: Board
starting = let c = Mask.fromCoords in Board
  { king   = c [e1, e8]
  , queen  = c [d1, e8]
  , rook   = c [a1, h1, a8, h8]
  , knight = c [b1, g1, b8, g8]
  , bishop = c [c1, f1, c8, f8]
  , pawn   = c [a2, b2, c2, d2, e2, f2, g2, h2, a7, b7, c7, d7, e7, f7, g7, h7]
  , white  = c [a1, b1, c1, d1, e1, f1, g1, h1, a2, b2, c2, d2, e2, f2, g2, h2]
  , black  = c [a7, b7, c7, d7, e7, f7, g7, h7, a8, b8, c8, d8, e8, f8, g8, h8]
  }

-- | Get a `Mask.Mask` indicating where pieces of a colour are.
get :: Board -> Colour -> Piece -> Mask.Mask
get b c King   = Mask.intersection (material b c) (king b)
get b c Queen  = Mask.intersection (material b c) (queen b)
get b c Rook   = Mask.intersection (material b c) (rook b)
get b c Knight = Mask.intersection (material b c) (knight b)
get b c Bishop = Mask.intersection (material b c) (bishop b)
get b c Pawn   = Mask.intersection (material b c) (pawn b)

-- | Makes all the squares on the mask blank.
setToBlank :: Board -> Mask.Mask -> Board
setToBlank b m = Board
  { king   = king   b `Mask.remove` m
  , queen  = queen  b `Mask.remove` m
  , rook   = rook   b `Mask.remove` m
  , knight = knight b `Mask.remove` m
  , bishop = bishop b `Mask.remove` m
  , pawn   = pawn   b `Mask.remove` m
  , white  = white  b `Mask.remove` m
  , black  = black  b `Mask.remove` m
  }

-- | Set the positions of a particular piece of a colour. This will overwrite
-- any pieces already at the locations indicated in the `Mask.Mask`.
set :: Board -> Colour -> Piece -> Mask.Mask -> Board
set b c p m = case p of
    King   -> updatedMaterial { king   = king   updatedMaterial `Mask.union` m }
    Queen  -> updatedMaterial { queen  = queen  updatedMaterial `Mask.union` m }
    Rook   -> updatedMaterial { rook   = pawn   updatedMaterial `Mask.union` m }
    Knight -> updatedMaterial { knight = knight updatedMaterial `Mask.union` m }
    Bishop -> updatedMaterial { bishop = bishop updatedMaterial `Mask.union` m }
    Pawn   -> updatedMaterial { pawn   = pawn   updatedMaterial `Mask.union` m }
  where
    blanked = setToBlank b m
    updatedMaterial = case c of
      White -> blanked { white = white blanked `Mask.union` m}
      Black -> blanked { black = black blanked `Mask.union` m}
