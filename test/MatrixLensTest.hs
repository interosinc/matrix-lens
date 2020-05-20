{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module MatrixLensTest
  ( spec_diag
  , spec_elemAt
  , spec_inverted
  , spec_minor
  , spec_row
  , spec_sub
  ) where

import Prelude

import Control.Lens          ( (%~)
                             , (&)
                             , (.~)
                             , (^.)
                             , (^?)
                             )
import Data.Foldable         ( traverse_ )
import Data.Matrix           ( Matrix
                             , fromLists
                             , transpose
                             )
import Data.Matrix.Lens
import Data.Ratio            ( (%)
                             , Ratio
                             )
import Data.Vector      as V ( fromList )
import Test.Tasty.Hspec

spec_elemAt :: Spec
spec_elemAt = describe "elemAt" $ do
  let m = exampleInt

  context "views the appropriate locations" $ do

    let testView (pair, value) = let (label, p) = setup pair in
          it ("at " <> label) $
            m ^. elemAt p `shouldBe` value

    traverse_ testView $
      [ ((1, 1), 1)
      , ((1, 2), 2)
      , ((2, 1), 4)
      , ((2, 2), 5)
      ]

  context "sets the appropriate locations" $ do
    let testSet (pair, expected) = let (label, p) = setup pair in
          it ("at " <> label) $
            (m & elemAt p .~ 99) `shouldBe` fromLists expected

    traverse_ testSet $
      [ ((1, 1), [ [99, 2, 3]
                 , [4,  5, 6]
                 , [7,  8, 9]
                 ])
      , ((1, 2), [ [1, 99, 3]
                 , [4,  5, 6]
                 , [7,  8, 9]
                 ])
      , ((2, 1), [ [1,  2, 3]
                 , [99, 5, 6]
                 , [7,  8, 9]
                 ])
      , ((2, 2), [ [1,  2, 3]
                 , [4, 99, 6]
                 , [7,  8, 9]
                 ])
      ]

spec_row :: Spec
spec_row = describe "row" $ do
  let m = exampleInt

  it "views the appropriate rows" $ do
    m ^. row 1 `shouldBe` V.fromList [1, 2, 3]

spec_sub :: Spec
spec_sub = describe "sub" $ do
  let m = exampleInt

  it "sets the appropriate locations" $
    (m & sub (2, 2) (3, 3) .~ m) `shouldBeMatrix`
      [ [1, 2, 3]
      , [4, 1, 2]
      , [7, 4, 5]
      ]

  it "modifies the appropriate locations" $
    (m & sub (2, 2) (3, 3) %~ transpose) `shouldBeMatrix`
      [ [1, 2, 3]
      , [4, 5, 8]
      , [7, 6, 9]
      ]

spec_minor :: Spec
spec_minor = describe "minor" $ do
  let (label, pair) = setup (1, 1) in context label $ do
    it ("reads the appropriate locations " <> label) $
      exampleInt ^. minor pair `shouldBeMatrix`
        [ [ 5, 6 ]
        , [ 8, 9 ]
        ]

    it ("writes the approprite locations " <> label) $
      (exampleInt & minor pair %~ transpose) `shouldBeMatrix`
        [ [ 1, 2, 3 ]
        , [ 4, 5, 8 ]
        , [ 7, 6, 9 ]
        ]

  let (label, pair) = setup (2, 2) in context label $ do

    it ("reads the appropriate locations" <> label) $
      exampleInt ^. minor pair `shouldBeMatrix`
        [ [ 1, 3 ]
        , [ 7, 9 ]
        ]

    it ("writes the approprite locations " <> label) $
      (exampleInt & minor pair %~ transpose) `shouldBeMatrix`
        [ [ 1, 2, 7 ]
        , [ 4, 5, 6 ]
        , [ 3, 8, 9 ]
        ]

  let (label, pair) = setup (1, 2) in context label $ do

    it ("reads the appropriate locations" <> label) $
      exampleInt ^. minor pair `shouldBeMatrix`
        [ [ 4, 6 ]
        , [ 7, 9 ]
        ]

    it ("writes the approprite locations " <> label) $
      (exampleInt & minor pair %~ transpose) `shouldBeMatrix`
        [ [ 1, 2, 3 ]
        , [ 4, 5, 7 ]
        , [ 6, 8, 9 ]
        ]

spec_inverted :: Spec
spec_inverted = describe "inverted" $ do
  it "inverts an invertible matrix" $
    exampleInvertible ^? inverted `shouldBeJustMatrix`
      [ [ 0 % 1, 1 % 5  ]
      , [ 1 % 1, 3 % 5 ]
      ]

  it "roundtrips" $
    exampleInvertible ^? inverted . inverted `shouldBe` Just
      exampleInvertible

  it "modifies correctly" $
    -- TODO confirm this is actually correct?
    (exampleInvertible & inverted . elemAt (1, 1) .~ 5 % 2) `shouldBeMatrix`
      [ [ 6 % 13, (-2) % 13 ]
      , [ (-10) % 13, 25 % 13 ]
      ]

spec_diag :: Spec
spec_diag = describe "diag" $ do
  context "given a square matrix" $ do
    let matrix = exampleInt

    it "reads the right values" $
      matrix ^. diag `shouldBe` V.fromList [1, 5, 9]

    it "writes the right values" $
      (matrix & diag .~ V.fromList [20, 60, 100]) `shouldBeMatrix`
        [ [ 20,  2,   3 ]
        , [  4, 60,   6 ]
        , [  7,  8, 100 ]
        ]

  context "given a non-square matrix" $ do
    let matrix = exampleNotSquare

    it "reads the right values" $ do
      matrix ^. diag `shouldBe` V.fromList [10, 50, 90]

-- ================================================================ --

infix 1 `shouldBeMatrix`
shouldBeMatrix :: (Eq a, Show a) => Matrix a -> [[a]] -> Expectation
shouldBeMatrix x y = x `shouldBe` fromLists y

infix 1 `shouldBeJustMatrix`
shouldBeJustMatrix :: (Eq a, Show a) => Maybe (Matrix a) -> [[a]] -> Expectation
shouldBeJustMatrix x y = x `shouldBe` Just (fromLists y)

setup :: (Int, Int) -> (String, (Int, Int))
setup = (,) =<< show

-- ================================================================ --

exampleInt :: Matrix Int
exampleInt = fromLists
  [ [1, 2, 3]
  , [4, 5, 6]
  , [7, 8, 9]
  ]

exampleInvertible :: Matrix (Ratio Int)
exampleInvertible = fromLists
  [ [ -3, 1 ]
  , [  5, 0 ]
  ]

exampleNotSquare :: Matrix Int
exampleNotSquare = fromLists
  [ [10,   20,  30]
  , [40,   50,  60]
  , [70,   80,  90]
  , [100, 110, 120]
  ]

_exampleFloat :: Matrix Float
_exampleFloat = fromIntegral <$> exampleInt
