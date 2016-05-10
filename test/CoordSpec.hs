module CoordSpec (spec) where

import Test.Hspec

import qualified Game.Rook.Coord as Coord

import Data.Bits (bit)

-- , toWord
-- , allCoords

spec = do
  describe "allCoords" $
    it "should contain all coords" $
      map Coord.toWord Coord.allCoords `shouldMatchList` map bit [0..63]
