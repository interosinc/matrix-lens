{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module MatrixLensTest
  ( hprop_squareMatricesHaveDeterminants
  , hprop_nonSquareMatricesHaveNoDeterminants
  , spec_determinant
  , spec_diag
  , spec_elemAt
  , spec_examples
  , spec_inverted
  , spec_minor
  , spec_row
  , spec_sub
  ) where

import           Prelude

import           Control.Lens               ( (%~)
                                            , (&)
                                            , (.~)
                                            , (^.)
                                            , (^?)
                                            , partsOf
                                            , set
                                            )
import           Control.Monad              ( guard )
import           Data.Foldable              ( traverse_ )
import           Data.Matrix                ( Matrix )
import qualified Data.Matrix      as Matrix
import           Data.Matrix.Lens
import           Data.Maybe                 ( isJust )
import           Data.Ratio                 ( (%)
                                            , Ratio
                                            )
import           Hedgehog                   ( Gen
                                            , Property
                                            , assert
                                            , forAll
                                            , property
                                            )
import qualified Hedgehog.Gen     as Gen
import qualified Hedgehog.Range   as Range
import           Test.Tasty.Hspec

spec_elemAt :: Spec
spec_elemAt = do
  let m = exampleInt

  context "views the appropriate locations" $ do

    let testView (pair, value) = let (label, p) = setup pair in
          it ("at " <> label) $
            m ^. elemAt p `shouldBe` value

    traverse_ testView
      [ ((1, 1), 1)
      , ((1, 2), 2)
      , ((2, 1), 4)
      , ((2, 2), 5)
      ]

  context "sets the appropriate locations" $ do
    let testSet (pair, expected) = let (label, p) = setup pair in
          it ("at " <> label) $
            (m & elemAt p .~ 99) `shouldBe` Matrix.fromLists expected

    traverse_ testSet
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
spec_row = do
  let m = exampleInt

  it "views the appropriate rows" $
    m ^. row 1 `shouldBe` [1, 2, 3]

spec_sub :: Spec
spec_sub = do
  let m = exampleInt

  it "sets the appropriate locations" $
    (m & sub (2, 2) (3, 3) .~ m) `shouldBeMatrix`
      [ [1, 2, 3]
      , [4, 1, 2]
      , [7, 4, 5]
      ]

  it "modifies the appropriate locations" $
    (m & sub (2, 2) (3, 3) %~ Matrix.transpose) `shouldBeMatrix`
      [ [1, 2, 3]
      , [4, 5, 8]
      , [7, 6, 9]
      ]

spec_minor :: Spec
spec_minor = do

  context "reads the appropriate locations" $ do

    let testView (pair, value) = let (label, p) = setup pair in
          it ("at " <> label) $
            exampleInt ^. minor p `shouldBeMatrix` value

    traverse_ testView
      [ ((1, 1), [ [5, 6]
                 , [8, 9]
                 ])
      , ((2, 2), [ [1, 3]
                 , [7, 9]
                 ])
      , ((1, 2), [ [4, 6]
                 , [7, 9]
                 ])
      ]

  context "sets the appropriate locations" $ do

    let testSet (pair, value) = let (label, p) = setup pair in
          it ("at " <> label) $
            (exampleInt & minor p %~ Matrix.transpose) `shouldBeMatrix` value

    traverse_ testSet
      [ ((1, 1), [ [ 1, 2, 3 ]
                 , [ 4, 5, 8 ]
                 , [ 7, 6, 9 ]
                 ])
      , ((2, 2), [ [ 1, 2, 7 ]
                 , [ 4, 5, 6 ]
                 , [ 3, 8, 9 ]
                 ])
      , ((1, 2), [ [ 1, 2, 3 ]
                 , [ 4, 5, 7 ]
                 , [ 6, 8, 9 ]
                 ])
      ]

spec_inverted :: Spec
spec_inverted = do
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
spec_diag = do
  context "given a square matrix" $ do
    let m = exampleInt

    it "reads the right values" $
      m ^. diag `shouldBe` [1, 5, 9]

    it "writes the right values" $
      (m & diag .~ [20, 60, 100]) `shouldBeMatrix`
        [ [ 20,  2,   3 ]
        , [  4, 60,   6 ]
        , [  7,  8, 100 ]
        ]

  context "given a non-square matrix" $ do
    let m = exampleNotSquare

    it "reads the right values" $
      m ^. diag `shouldBe` [10, 50, 90]


    it "writes the right values" $
      (m & diag .~ [1, 2, 3]) `shouldBeMatrix`
        [ [  1,  20,  30]
        , [ 40,   2,  60]
        , [ 70,  80,   3]
        , [100, 110, 120]
        ]

spec_examples :: Spec
spec_examples = do
  it "should be able to transpose a minor matrix" $
    (exampleInt & minor (1, 1) %~ Matrix.transpose) `shouldBeMatrix`
      [ [1, 2, 3]
      , [4, 5, 8]
      , [7, 6, 9]
      ]

  it "should be able to reverse rows" $
    (exampleInt & rows %~ reverse) `shouldBeMatrix`
      [ [7, 8, 9]
      , [4, 5, 6]
      , [1, 2, 3]
      ]

  it "should be able to reverse cols" $
    (exampleInt & cols %~ reverse) `shouldBeMatrix`
      [ [3, 2, 1]
      , [6, 5, 4]
      , [9, 8, 7]
      ]

  it "should be able to set a minor matrix to one value" $
    (exampleInt & minor (2, 2) . flattened .~ 1) `shouldBeMatrix`
      [ [1, 2, 1]
      , [4, 5, 6]
      , [1, 8, 1]
      ]

  it "should be able to set everything top to bottom" $
    (exampleInt & partsOf flattened .~ [90,80..]) `shouldBeMatrix`
      [ [90, 80, 70]
      , [60, 50, 40]
      , [30, 20, 10]
      ]

  it "should be able to reverse all cells" $
    (exampleInt & partsOf flattened %~ reverse) `shouldBeMatrix`
      [ [9, 8, 7]
      , [6, 5, 4]
      , [3, 2, 1]
      ]

spec_determinant :: Spec
spec_determinant = do
  let m3x3 = Matrix.fromLists
               [ [6,  1, 1 :: Int]
               , [4, -2, 5]
               , [2,  8, 7]
               ]
      m2x2 = m3x3 ^. minor (1, 1)

  it "should return Nothing on non-square matrices" $
    exampleNotSquare ^. determinant `shouldBe` Nothing

  it "should work for 2x2 matrices" $
    m2x2 ^. determinant `shouldBe` Just (-54)

  it "should work for > 2x2 square matrices" $
    m3x3 ^. determinant `shouldBe` Just (-306)

hprop_squareMatricesHaveDeterminants :: Property
hprop_squareMatricesHaveDeterminants = property $ do
  m <- forAll genSquareMatrix
  assert . isJust $ m ^. determinant

hprop_nonSquareMatricesHaveNoDeterminants :: Property
hprop_nonSquareMatricesHaveNoDeterminants = property $ do
  m <- forAll genNonSquareMatrix
  assert . not . isJust $ m ^. determinant

genSquareMatrix :: Gen (Matrix Int)
genSquareMatrix = do
  sz <- genSize
  (flip (set (partsOf flattened)) $ Matrix.identity sz) <$> genValues sz
  where
    genSize     = Gen.integral (Range.linear 2 10)
    genValues n = Gen.list (Range.singleton $ n * n) genInt
    genInt      = Gen.integral (Range.linearFrom 0 (-100) 100)

genNonSquareMatrix :: Gen (Matrix Int)
genNonSquareMatrix = do
  r <- genSize
  c <- genSize
  guard $ r /= c
  let (lo, hi) = (min r c, max r c)
      idm      = Matrix.identity lo
      m        = Matrix.extendTo 0 r c idm
  (flip (set (partsOf flattened)) $ m) <$> genValues hi
  where
    genSize     = Gen.integral (Range.linear 2 10)
    genValues n = Gen.list (Range.singleton $ n * n) genInt
    genInt      = Gen.integral (Range.linearFrom 0 (-100) 100)

infix 1 `shouldBeMatrix`
shouldBeMatrix :: (Eq a, Show a) => Matrix a -> [[a]] -> Expectation
shouldBeMatrix x y = x `shouldBe` Matrix.fromLists y

infix 1 `shouldBeJustMatrix`
shouldBeJustMatrix :: (Eq a, Show a) => Maybe (Matrix a) -> [[a]] -> Expectation
shouldBeJustMatrix x y = x `shouldBe` Just (Matrix.fromLists y)

setup :: (Int, Int) -> (String, (Int, Int))
setup = (,) =<< show

-- ================================================================ --

exampleInt :: Matrix Int
exampleInt = Matrix.fromLists
  [ [1, 2, 3]
  , [4, 5, 6]
  , [7, 8, 9]
  ]

exampleInvertible :: Matrix (Ratio Int)
exampleInvertible = Matrix.fromLists
  [ [ -3, 1 ]
  , [  5, 0 ]
  ]

exampleNotSquare :: Matrix Int
exampleNotSquare = Matrix.fromLists
  [ [10,   20,  30]
  , [40,   50,  60]
  , [70,   80,  90]
  , [100, 110, 120]
  ]

_exampleFloat :: Matrix Float
_exampleFloat = fromIntegral <$> exampleInt
