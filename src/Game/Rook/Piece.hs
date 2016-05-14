-- |
-- Module      : Game.Rook.Piece
-- Description : Chess Pieces and related information
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--

module Game.Rook.Piece where

data Colour = White | Black deriving (Show, Eq)

data Piece = Rook | Knight | Bishop | Queen | King | Pawn deriving (Show, Eq)
