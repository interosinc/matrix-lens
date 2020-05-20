module MatrixLensTest ( spec_sub ) where

import Prelude

import Control.Lens     ( (&)
                        , (.~)
                        )
import Data.Matrix      ( Matrix
                        , fromLists
                        )
import Data.Matrix.Lens
-- import Data.Ratio       ( Ratio )
import Test.Tasty.Hspec

spec_sub :: Spec
spec_sub = describe "sub" $
  it "sets the appropriate locations" $
    (exampleInt & sub (2, 2) (3, 3) .~ exampleInt)
      `shouldBe` fromLists [ [1, 2, 3]
                           , [4, 1, 2]
                           , [7, 4, 5]
                           ]


-- ================================================================ --

exampleInt :: Matrix Int
exampleInt = fromLists
  [ [1, 2, 3]
  , [4, 5, 6]
  , [7, 8, 9]
  ]

-- exampleNotSquare :: Matrix Int
-- exampleNotSquare = fromLists
--   [ [1,   2,  3]
--   , [4,   5,  6]
--   , [7,   8,  9]
--   , [10, 11, 12]
--   ]

-- exampleFloat :: Matrix Float
-- exampleFloat = fromIntegral <$> exampleInt

-- exampleInvertible :: Matrix (Ratio Int)
-- exampleInvertible = fromLists
--   [ [ -3, 1 ]
--   , [  5, 0 ]
--   ]
