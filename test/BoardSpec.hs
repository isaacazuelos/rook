module BoardSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Game.Rook.Piece
import Game.Rook.Coord

import qualified Game.Rook.Mask as Mask
import qualified Game.Rook.Board as Board

instance Arbitrary Mask.Mask where
  arbitrary = fmap Mask.fromWord arbitrary

spec = do
  let file1 = Mask.fromCoords [a1, b1, c1, d1, e1, f1, g1, h1]
  let file2 = Mask.fromCoords [a2, b2, c2, d2, e2, f2, g2, h2]
  describe "empty" $
    it "shouldn't have any pieces on it" $ do
      Board.material Board.empty White `shouldBe` Mask.empty
      Board.material Board.empty Black `shouldBe` Mask.empty
  describe "material" $
    it "should know what the material for a colour is" $
      Board.material Board.starting White `shouldBe` Mask.union file1 file2
  describe "get" $
    it "should get the pieces of a type and colour" $
      Board.get Board.starting White Pawn `shouldBe` file2
  describe "set" $ do
    it "should set where a type of piece are." $
      property (\ m -> let newBoard = Board.set Board.empty White Pawn m in do
        Board.material newBoard White  `shouldBe` m
        Board.get newBoard White Pawn  `shouldBe` m
        Board.get newBoard Black Pawn  `shouldBe` Mask.empty
        Board.get newBoard White Queen `shouldBe` Mask.empty)
    it "should remove pieces of other types when overwriting" $
      property $ \ m -> do
        let board1 = Board.set Board.empty White Pawn m
        let board2 = Board.set board1      Black Rook m
        Board.get board2 White Pawn `shouldBe` Mask.empty
        Board.get board2 Black Rook `shouldBe` m
