-- |
-- Module      : Game.Rook.Piece
-- Description : Chess Pieces and related information
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--

module Game.Rook.Piece where

-- | The two colours relevant to chess.
data Colour = White | Black deriving (Show, Eq)

-- | The six different kinds of pieces that can be on a chess board.
data Piece = Rook | Knight | Bishop | Queen | King | Pawn deriving (Show, Eq)

-- | The two sides which exist relative to each player.
data Side = Queenside | Kingside deriving (Show, Eq)
