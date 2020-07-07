{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Matrix.Lens
  ( flattened
  , col
  , cols
  , determinant
  , diag
  , elemAt
  , inverted
  , isSquare
  , minor
  , resized
  , row
  , rows
  , scaled
  , scalingRow
  , sub
  , size
  , slidingCols
  , slidingRows
  , switchingCols
  , switchingRows
  , transposed
  ) where

import           Prelude

import           Control.Lens                            hiding ( set )
import           Data.Bifunctor                                 ( first )
import qualified Data.Foldable                      as F        ( toList )
import qualified Data.List                          as L
import           Data.Matrix
import           Data.Matrix.Lens.Internal          as X        ( col
                                                                , elemAt
                                                                , getSize
                                                                , isSquare
                                                                , minor
                                                                , row
                                                                , rows
                                                                , slidingCols
                                                                , slidingRows
                                                                , switchingCols
                                                                , switchingRows
                                                                )
import           Data.Matrix.Lens.Internal.Warnings             ( determinant
                                                                , size
                                                                )
import           Data.Maybe                                     ( fromMaybe )
import qualified Data.Vector                        as V

transposed :: Iso' (Matrix a) (Matrix a)
transposed = iso transpose transpose

scaled :: Num a => a -> Iso' (Matrix a) (Matrix a)
scaled n = iso (scaleMatrix n) (scaleMatrix . negate $ n)

cols :: Lens' (Matrix a) [[a]]
cols = lens (L.transpose . toLists) (const (fromLists . L.transpose))

scalingRow :: Num a => Int -> a -> Iso' (Matrix a) (Matrix a)
scalingRow r n = iso (scaleRow n r) (scaleRow (negate n) r)

flattened :: Traversal' (Matrix a) a
flattened = rows . each . each

sub :: (Int, Int) -> (Int, Int) -> Lens' (Matrix a) (Matrix a)
sub (r1, c1) (r2, c2) = lens (submatrix r1 r2 c1 c2) (setSubMatrix (r1, c1))
  where
    setSubMatrix (r, c) dst src = foldr f dst indexedRows
      where
        indexedRows = zip [r..] . map (zip [c..]) . toLists $ src
        f (r', indexedCols) dst' = foldr (g r') dst' indexedCols
        g r' (c', x) dst' = fromMaybe dst' $ safeSet x (r', c') dst'

inverted :: (Eq a, Fractional a) => Prism' (Matrix a) (Matrix a)
inverted = flip prism (\x -> first (const x) . inverse $ x) $ \x -> case inverse x of
  Left  _ -> x
  Right y -> y

diag :: Lens' (Matrix a) [a]
diag = lens (V.toList . getDiag) (\m -> setDiag m . V.fromList)
  where
    setDiag m = foldr f m . zip [1..] . F.toList
      where
        f (n, x) m' = m' & elemAt (n, n) .~ x


resized :: a -> (Int, Int) -> Lens' (Matrix a) (Matrix a)
resized e (r, c) = lens (setSize e r c) (uncurry (setSize e) . getSize)
