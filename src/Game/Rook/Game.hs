{-# LANGUAGE BinaryLiterals #-}

-- |
-- Module      : Game.Rook.Game
-- Description : Game Representation
-- License     : MIT License
-- Maintainer  : Isaac Azuelos
--
-- This is our game representation. It's a `Board.Board` paired with some bits
-- to encode other status flags.
--
-- There are a few things to keep track of, each with their bit requirements.
--
-- 1. Who's ply is it anyway? We'll use 0 for white and 1 for black. (1 bit)
-- 2. Which castling options remain open? We'll use 1 bit for each corner.
--    (4 bits)
-- 3. Fifty move rule status, so a number between 0 and 50. We'll need to mask
--    and shift to recover or set. (6 bits)
-- 4. En Passant coordinate, if any. There are only 16 possible values,
--    but we need another flag to represent a non-option. Again, this is a
--    number we'll need to mask and shift to recover.(4+1 bits)
--
-- So we'll need a total of 16 bits. That works out nicely!
--
-- We'll be using the following bitmask.
--  @
--  e = en Passant coord encoding
--  E = Is there an en Passant coord in the `e` bits?
--  W = White Kingside calste
--  w = White Queenside castle
--  B = Black Kingside castle
--  b = Black Queenside castle
--  t = Who's ply is it?
--  f = Fifty move rule status
--
--    F E D C B A 9 8   7 6 5 4 3 2 1 0
--   +---------------+ +---------------+
--   |f|f|f|f|f|f|b|B| |w|W|t|E|e|e|e|e|
--   +---------------+ +---------------+
-- @
--
-- Since we're using a single `Word16` for our flags, we're going to keep the
-- the implementation completely strict, to save memory.

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
    , FiftyMoveException (TooLowException, TooHighException)
    , fiftyMoveStatus
    , setFiftyMoveStatus
    -- * enPassant state
    , EnPassantException (EnPassantException)
    , enPassantOption
    , setEnPassantOption
    )
  where

import           Control.Exception
import           Data.Maybe        (fromMaybe)
import           Data.Typeable

import           Game.Rook.Coord
import           Game.Rook.Piece

import           Data.Bits
import           Data.Word         (Word16)

import qualified Game.Rook.Board   as Board

type Status = Word16

-- | A `Game` is a chess board as well as all the supplementary information
-- about the game, such as:
--
-- 1. which colour's ply comes next.
-- 2. the remaining castling options.
-- 3. fifty move rule status.
-- 4. en Passant status.
data Game = Game !Board.Board !Status
  deriving (Show, Eq)

-- | `FiftyMoveException`s are thrown when an invalid fifty move rule state state
-- would be created, either becuase the number of moves is over 50 and the game
-- must have drawn, or becuase a negative value was set.
data FiftyMoveException
  = TooLowException Int
  | TooHighException Int
  deriving (Typeable, Eq)

instance Show FiftyMoveException where
 show (TooLowException  n) = "FiftyMoveException: value too low "  ++ show n
 show (TooHighException n) = "FiftyMoveException: value too high " ++ show n

instance Exception FiftyMoveException

-- | `EnPassantException` is thrown when an invalid coordinate is set. En
-- Passant moves can only happen on the 3rd and 6th rank.
data EnPassantException = EnPassantException Coord
  deriving (Typeable, Eq)

instance Show EnPassantException where
  show (EnPassantException c) = "EnPassantException: invalid coord " ++ show c

instance Exception EnPassantException

-- | An empty game, with a blank board, White's turn, no castle options, zero on
-- the fifty move clock, and no en Passant square.
empty :: Game
empty = Game Board.empty 0

-- | A board with the typical starting positioins for chess.
starting :: Game
starting = Game Board.starting 0b0000001111000000

-- | The board state in a game.
board :: Game -> Board.Board
board (Game b _) = b

-- | Set the board in a game.
setBoard :: Game -> Board.Board -> Game
setBoard (Game _ s) b = Game b s

-- | Who's turn is it?
turn :: Game -> Colour
turn (Game _ s) = if testBit s 0xA then Black else White

-- | Set who gets to move next.
setTurn :: Game -> Colour -> Game
setTurn (Game b s) c = Game b s'
  where s' = (if c == Black then setBit else clearBit) s 0xA

-- | The bit indicies used for the different castle options.
castleIndex :: Colour -> Side -> Int
castleIndex White Kingside  = 0x6
castleIndex White Queenside = 0x7
castleIndex Black Kingside  = 0x8
castleIndex Black Queenside = 0x9

-- | Is a particular castle option available?
castlingOption :: Game -> Colour -> Side -> Bool
castlingOption (Game _ s) c sd = testBit s (castleIndex c sd)

-- | Set the availability of a castling option.
setCastlingOption :: Game -> Colour -> Side -> Bool -> Game
setCastlingOption (Game b s) c sd f = Game b s'
  where
    i  = castleIndex c sd
    s' = (if f then setBit else clearBit) s i

-- | Set the complete castle state at once. Any `(Colour, Side)` in the list is
-- set as available and any missing is set as unavailable. Repeated values are
-- just set as available.
setCastlingOptions :: Game -> [(Colour, Side)] -> Game
setCastlingOptions (Game b s) enabled = Game b s'
  where
    allUnset = s .&. 0b1111110000111111
    s' = foldl (\ bits -> setBit bits . uncurry castleIndex) allUnset enabled

-- The `fiftyMoveStatus` and `enPassantOption` status flags are similar, in that
-- they're numbers. So we need to mask them out, shift them to the LSB, and
-- convert them to a proper `Int`.

-- | The bits used by the fifty move rule tracking.
fiftyMoveBits :: Word16
fiftyMoveBits = 0b1111110000000000

-- | The fifty move rule status of the game.
fiftyMoveStatus :: Game -> Int
fiftyMoveStatus (Game _ s) = fromIntegral $ shiftR (s .&. fiftyMoveBits) 10

-- | Set the fifty move rule status of the game. This will throw if the number
-- isn't in the range @[0..50]@.
setFiftyMoveStatus :: Game -> Int -> Game
setFiftyMoveStatus (Game b s) n
    | n > 50    = throw $ TooHighException n
    | n < 0     = throw $ TooLowException  n
    | otherwise = Game b s'
  where
    s' = (s .&. complement fiftyMoveBits) .|. shiftL (fromIntegral n) 10

-- | Bits used to track the en passant state.
enPassantBits :: Word16
enPassantBits = 0b0000000000011111

-- | All the coords that en Passant moves can be placed on. Note that the order
-- doesn't matter, since this list is used for both setting and getting the
-- values.
enPassantCoords :: [Coord]
enPassantCoords = [ a3, b3, c3, d3, e3, f3, g3, h3
                  , a6, b6, c6, d6, e6, f6, g6, h6 ]

-- | The en passant square. Returns `Nothing` if none is available.
enPassantOption :: Game -> Maybe Coord
enPassantOption (Game _ s) =
  if testBit s 0x5
    then Just $ enPassantCoords !! fromIntegral (enPassantBits .&. s)
    else Nothing

-- | Set a square to be the en passant option. This will throw an
-- `EnPassantException` if the square is invalid.
setEnPassantOption :: Game -> Maybe Coord -> Game
setEnPassantOption (Game b s) Nothing  = Game b (clearBit s 0x5)
setEnPassantOption (Game b s) (Just c) = Game b (setBit s' 0x5)
      where
        s' = (s .&. complement enPassantBits) .|. index
        index = fromMaybe (throw $ EnPassantException c)
                          (lookup c (zip enPassantCoords [0 :: Word16 .. ]))
