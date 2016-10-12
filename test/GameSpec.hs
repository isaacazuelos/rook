module GameSpec (spec) where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Exception (evaluate)

import           Game.Rook.Piece
import           Game.Rook.Coord

import qualified Game.Rook.Board   as Board
import qualified Game.Rook.Game    as Game

spec = do
  describe "empty" $ do
    it "has an empty board" $
      Game.board Game.empty `shouldBe` Board.empty
    it "should be White's turn" $
      Game.turn Game.empty `shouldBe` White
    it "has no castling options" $ do
      Game.castlingOption Game.empty White Kingside  `shouldBe` False
      Game.castlingOption Game.empty White Queenside `shouldBe` False
      Game.castlingOption Game.empty Black Kingside  `shouldBe` False
      Game.castlingOption Game.empty Black Queenside `shouldBe` False
    it "is zero moves into the fifty move rule" $
      Game.fiftyMoveStatus Game.empty `shouldBe` 0
    it "has no enPassant square set" $
      Game.enPassantOption Game.empty `shouldBe` Nothing

  describe "starting" $ do
    it "is the starting board" $
      Game.board Game.starting `shouldBe` Board.starting
    it "should be White's turn" $
      Game.turn Game.starting `shouldBe` White
    it "has all castling options available" $ do
      Game.castlingOption Game.starting White Kingside  `shouldBe` True
      Game.castlingOption Game.starting White Queenside `shouldBe` True
      Game.castlingOption Game.starting Black Kingside  `shouldBe` True
      Game.castlingOption Game.starting Black Queenside `shouldBe` True
    it "is zero moves into the fifty move rule" $
      Game.fiftyMoveStatus Game.starting `shouldBe` 0
    it "has no enPassant square set" $
      Game.enPassantOption Game.starting `shouldBe` Nothing


  describe "board" $
    it "should be the board from a game" $
      Game.board (Game.setBoard Game.empty Board.starting) `shouldBe` Board.starting

  describe "turn" $
    it "should be the player that goes next " $
      Game.turn (Game.setTurn Game.empty Black) `shouldBe` Black

  describe "castlingOption" $ do
    it "should enable castling options" $ do
      let enabled = Game.setCastlingOption Game.empty White Queenside True
      Game.castlingOption enabled White Queenside  `shouldBe` True
      Game.castlingOption enabled White Kingside   `shouldBe` False
      Game.castlingOption enabled Black Queenside  `shouldBe` False
      Game.castlingOption enabled Black Kingside   `shouldBe` False
    it "should disable castling options" $ do
      let disabled = Game.setCastlingOption Game.starting White Queenside False
      Game.castlingOption disabled White Queenside `shouldBe` False
      Game.castlingOption disabled White Kingside  `shouldBe` True
      Game.castlingOption disabled Black Queenside `shouldBe` True
      Game.castlingOption disabled Black Kingside  `shouldBe` True

  describe "setCastlingOptions" $ do
    let testSides = [ (White, Queenside), (Black, Kingside) ]
    let board = Game.setCastlingOptions Game.starting testSides
    it "should enable the options specified" $ do
      Game.castlingOption board White Queenside `shouldBe` True
      Game.castlingOption board Black Kingside  `shouldBe` True
    it "should disable the options not specified" $ do
      Game.castlingOption board White Kingside  `shouldBe` False
      Game.castlingOption board Black Queenside `shouldBe` False
    it "should ignore options specified twice" $ do
      let testSides2 = [ (White, Kingside), (White, Kingside) ]
      let board' = Game.setCastlingOptions Game.empty testSides2
      Game.castlingOption board' White Queenside `shouldBe` False
      Game.castlingOption board' Black Kingside  `shouldBe` False
      Game.castlingOption board' White Kingside  `shouldBe` True
      Game.castlingOption board' Black Queenside `shouldBe` False


  describe "fiftyMoveStatus" $ do
    it "should be the 50-move-rule status" $
      Game.fiftyMoveStatus (Game.setFiftyMoveStatus Game.empty 5) `shouldBe` 5
    it "should throw on inputs below 0" $ do
      let pureExpr = Game.setFiftyMoveStatus Game.empty (-123)
      evaluate pureExpr `shouldThrow` (== Game.TooLowException (-123))
    it "should accept 50 as a value to indicate a fifty-move-rule draw." $
      Game.fiftyMoveStatus (Game.setFiftyMoveStatus Game.empty 50) `shouldBe` 50

    it "should throw on inputs above 50" $ do
      let pureExpr = Game.setFiftyMoveStatus Game.empty 111
      evaluate pureExpr `shouldThrow` (== Game.TooHighException 111)

  describe "enPassantOption" $ do
    it "should be the status" $ do
      let board = Game.setEnPassantOption Game.empty (Just a3)
      let expected = Just a3
      Game.enPassantOption board `shouldBe` expected
    let rank3 = [a3, b3, c3, d3, e3, f3, g3, h3]
    let rank6 = [a6, b6, c6, d6, e6, f6, g6, h6]
    let accept = rank3 ++ rank6
    let reject = filter (`notElem` accept) allCoords
    it "should only accept the 3rd and 6th rank coords" $ do
      let result = Game.enPassantOption . Game.setEnPassantOption Game.empty
      let isAccepted = shouldBe =<< result
      mapM_ (isAccepted . Just) accept
    it "should throw on other ranks" $ do
      -- Note `evaluate` does WHNF, since that might cause these tests to fail
      -- if there are strictness changes.
      let result = evaluate . Game.setEnPassantOption Game.empty . Just
      let isRejected a = result a `shouldThrow` (== Game.EnPassantException a)
      mapM_ isRejected reject
