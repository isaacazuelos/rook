module Game.Rook.Game
    ( Game
    , empty
    , starting
    , board
    , setBoard
    , fiftyMoveStatus
    , setFiftyMoveStatus
    , enPassantOption
    , setEnPassantOption
    , plyColour
    , setPlyColour
    , castlingOption
    , setCastlingOption
    , setCastlingOptions
    )
  where

import Game.Rook.Piece
import Game.Rook.Coord
import qualified Game.Rook.Board as Board

data Game = Game

empty :: Game
empty = undefined

starting :: Game
starting = undefined

board :: Game -> Board.Board
board = undefined

setBoard :: Game -> Board.Board -> Game
setBoard = undefined

fiftyMoveStatus :: Game -> Int
fiftyMoveStatus = undefined

setFiftyMoveStatus :: Game -> Int -> Game
setFiftyMoveStatus = undefined

enPassantOption :: Game -> Maybe Coord
enPassantOption = undefined

setEnPassantOption :: Game -> Maybe Coord -> Game
setEnPassantOption = undefined

plyColour :: Game -> Colour
plyColour = undefined

setPlyColour :: Game -> Colour -> Game
setPlyColour = undefined

castlingOption :: Game -> Colour -> Side -> Bool
castlingOption = undefined

setCastlingOption :: Game -> Colour -> Side -> Bool -> Game
setCastlingOption = undefined

setCastlingOptions :: Game -> [(Colour, Side)] -> Game
setCastlingOptions = undefined
