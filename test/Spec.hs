import Test.Hspec

import qualified CoordSpec
import qualified MaskSpec

main :: IO ()
main = hspec $
  describe "Rook" $ do
    describe "Coord" CoordSpec.spec
    describe "Mask"  MaskSpec.spec
