-- The state is ordered by the obscurity of the rules.

module Game.Rook.Game
    ( Game
    -- * Games
    , empty
    , starting
    -- * The game's board
    , board
    , setBoard
    -- * Who's turn is it anyway?
    , turn
    , setTurn
    -- * Castling availability
    , castlingOption
    , setCastlingOption
    , setCastlingOptions
    -- * Fifty Move Rule state
    , fiftyMoveStatus
    , setFiftyMoveStatus
    -- * enPassant state
    , enPassantOption
    , setEnPassantOption
    -- * Excetions
    , FiftyMoveException (FiftyMoveException)
    , EnPassantException (EnPassantException)
    )
  where

import Control.Exception
import Data.Typeable

import Game.Rook.Piece
import Game.Rook.Coord

import qualified Game.Rook.Board as Board

data Game = Game

data FiftyMoveException = FiftyMoveException Int
  deriving (Typeable, Eq)

instance Show FiftyMoveException where
 show (FiftyMoveException n) = "FiftyMoveException: invalid value " ++ show n

instance Exception FiftyMoveException

data EnPassantException = EnPassantException Coord
  deriving (Typeable, Eq)

instance Show EnPassantException where
  show (EnPassantException c) = "EnPassantException: invalid coord " ++ show c

instance Exception EnPassantException

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

turn :: Game -> Colour
turn = undefined

setTurn :: Game -> Colour -> Game
setTurn = undefined

castlingOption :: Game -> Colour -> Side -> Bool
castlingOption = undefined

setCastlingOption :: Game -> Colour -> Side -> Bool -> Game
setCastlingOption = undefined

setCastlingOptions :: Game -> [(Colour, Side)] -> Game
setCastlingOptions = undefined
