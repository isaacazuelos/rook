module MaskSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding ((.&.))

import Data.Bits

import qualified Game.Rook.Coord as Coord
import qualified Game.Rook.Mask as Mask

instance Arbitrary Mask.Mask where
  arbitrary = fmap Mask.fromWord arbitrary

spec = do
  describe "toWord and fromWord" $ do
    it "fromWord . toWord should do nothing" $
      property (\ m -> (Mask.fromWord . Mask.toWord) m `shouldBe` m)
    it "toWord . fromWord should do nothing" $
      property (\ m -> (Mask.toWord . Mask.fromWord) m `shouldBe` m)

  describe "fromCoords" $ do
    it "should produce an empty mask form the empty list" $
      Mask.fromCoords [] `shouldBe` Mask.empty
    it "should properly add coords from the list" $ do
      Mask.fromCoords [Coord.a1, Coord.f7] `shouldBe`
        Mask.fromWord (Coord.toWord Coord.a1 .|. Coord.toWord Coord.f7)
      Mask.fromCoords Coord.allCoords `shouldBe`
        Mask.fromWord (complement zeroBits)
    it "should produce a full mask when given all coords" $
      Mask.fromCoords Coord.allCoords `shouldBe` Mask.full

  describe "toCoords" $ do
    it "should produce the empty list for the empty mask" $
      Mask.toCoords Mask.empty `shouldBe` []
    it "should produce the coords used by fromCoords" $
      Mask.toCoords Mask.full `shouldMatchList` Coord.allCoords

  describe "empty" $
    it "should have nothing on it" $ do
      Mask.empty `shouldBe` Mask.fromWord 0
      Mask.toWord Mask.empty `shouldBe` 0

  describe "full" $
    it "should contain all coords" $ do
      Mask.full `shouldBe` Mask.fromCoords Coord.allCoords
      Mask.full `shouldBe` Mask.fromWord maxBound

  describe "isSet" $
    it "should be True when the coord is set" $ do
      Mask.isSet Mask.full Coord.a1 `shouldBe` True
      Mask.isSet Mask.empty Coord.a1 `shouldBe` False

  describe "intersection" $ do
    it "should treat empty like a zero" $
      property (\m -> Mask.intersection Mask.empty m `shouldBe` Mask.empty)
    it "should propertly find the intersection" $ do
      let m1 = Mask.fromCoords [Coord.a1, Coord.c4, Coord.f6]
      let m2 = Mask.fromCoords [Coord.c4, Coord.b7, Coord.f5]
      let expected = Mask.fromCoords [ Coord.c4 ]
      Mask.intersection m1 m2 `shouldBe` expected

  describe "union" $ do
    it "should treat full like a zero" $
      property (\m -> Mask.union Mask.full m `shouldBe` Mask.full)
    it "should propertly find the union" $ do
      let m1 = Mask.fromCoords [Coord.a1, Coord.c4, Coord.f6]
      let m2 = Mask.fromCoords [Coord.c4, Coord.b7, Coord.f5]
      let expected = Mask.fromCoords [ Coord.a1, Coord.c4, Coord.f6
                                     , Coord.c4, Coord.b7, Coord.f5 ]
      Mask.union m1 m2 `shouldBe` expected

  describe "invert" $ do
    it "should invert full to empty" $
      Mask.invert Mask.full `shouldBe` Mask.empty
    it "should invert empty to full" $
      Mask.invert Mask.empty `shouldBe` Mask.full
    it "should be it's own inverse" $
      property (\m -> (Mask.invert . Mask.invert) m `shouldBe` m)

  describe "remove" $ do
    it "should not be able to remove from empty" $
      property (\ m -> Mask.remove Mask.empty m `shouldBe` Mask.empty)
    it "should do nothing if removing empty" $
      property (\ m -> Mask.remove m Mask.empty `shouldBe` m)
    it "should be empty if removing full" $
      property (\ m -> Mask.remove m Mask.full `shouldBe` Mask.empty)
    it "should remove as expected" $ do
      let m1 = Mask.fromCoords [Coord.a1, Coord.a2]
      let m2 = Mask.fromCoords [Coord.a1, Coord.b1]
      let expected = Mask.fromCoords [Coord.a2]
      Mask.remove m1 m2 `shouldBe` expected
