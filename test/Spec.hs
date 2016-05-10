import Test.Hspec

import qualified MaskSpec

main :: IO ()
main = hspec $
  describe "Rook" $
    describe "Mask" MaskSpec.spec
