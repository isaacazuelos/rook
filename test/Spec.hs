import Test.Hspec

import qualified CoordSpec
import qualified MaskSpec
import qualified BoardSpec

main :: IO ()
main = hspec $
  describe "Rook" $ do
    describe "Coord" CoordSpec.spec
    describe "Mask"  MaskSpec.spec
    describe "Board"  BoardSpec.spec
