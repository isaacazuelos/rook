import Test.Hspec

import qualified CoordSpec
import qualified MaskSpec
import qualified BoardSpec
import qualified GameSpec

main :: IO ()
main = hspec $
  describe "Rook" $ do
    describe "Coord" CoordSpec.spec
    describe "Mask"  MaskSpec.spec
    describe "Board" BoardSpec.spec
    describe "Game"  GameSpec.spec 
