{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Matrix.Lens
  ( col
  , diag
  , elemAt
  , inverted
  , minor
  , row
  , scaled
  , scalingRow
  , sub
  , slidingCols
  , slidingRows
  , switchingCols
  , switchingRows
  , transposed
  ) where

import           Prelude

import           Control.Lens        hiding ( set )
import           Data.Bifunctor             ( first )
import           Data.Foldable  as F        ( toList )
import           Data.Matrix    as M
import           Data.Maybe                 ( fromMaybe )
import           Data.Vector                ( Vector )
import qualified Data.Vector    as V

elemAt :: (Int, Int) -> Lens' (Matrix a) a
elemAt (i, j) = lens (getElem i j) (\m x -> setElem x (i, j) m)

row :: Int -> Lens' (Matrix a) (Vector a)
row r = lens (getRow r) (setRow r)

col :: Int -> Lens' (Matrix a) (Vector a)
col c = lens (getCol c) (setCol c)

transposed :: Iso' (Matrix a) (Matrix a)
transposed = iso transpose transpose

scaled :: Num a => a -> Iso' (Matrix a) (Matrix a)
scaled n = iso (scaleMatrix n) (scaleMatrix . negate $ n)

scalingRow :: Num a => Int -> a -> Iso' (Matrix a) (Matrix a)
scalingRow r n = iso (scaleRow n r) (scaleRow (negate n) r)

switchingRows :: Int -> Int -> Iso' (Matrix a) (Matrix a)
switchingRows r1 r2 = iso (switchRows r1 r2) (switchRows r2 r1)

switchingCols :: Int -> Int -> Iso' (Matrix a) (Matrix a)
switchingCols c1 c2 = iso (switchCols c1 c2) (switchCols c2 c1)

slidingRows :: Int -> Int -> Iso' (Matrix a) (Matrix a)
slidingRows r1 r2 = iso (slideRows r1 r2) (slideRows r2 r1)

slidingCols :: Int -> Int -> Iso' (Matrix a) (Matrix a)
slidingCols c1 c2 = iso (slideCols c1 c2) (slideCols c2 c1)

sub :: (Int, Int) -> (Int, Int) -> Lens' (Matrix a) (Matrix a)
sub (r1, c1) (r2, c2) = lens (submatrix r1 r2 c1 c2) (setSubMatrix (r1, c1))

minor :: (Int, Int) -> Lens' (Matrix a) (Matrix a)
minor (r, c) = lens (minorMatrix r c) (setMinorMatrix (r, c))

inverted :: (Eq a, Fractional a) => Prism' (Matrix a) (Matrix a)
inverted = flip prism (\x -> first (const x) . inverse $ x) $ \x -> case inverse x of
  Left  _ -> x
  Right y -> y

diag :: Lens' (Matrix a) (Vector a)
diag = lens getDiag setDiag

-- ================================================================ --

setRow :: Int -> Matrix a -> Vector a -> Matrix a
setRow r m v = foldr ((\(c, x) -> setElem x (r, c))) m $
  zip [1..] (V.toList v)

setCol :: Int -> Matrix a -> Vector a -> Matrix a
setCol c m v = foldr ((\(r, x) -> setElem x (r, c))) m $
  zip [1..] (V.toList v)

setSubMatrix :: (Int, Int) -> Matrix a -> Matrix a -> Matrix a
setSubMatrix (r, c) dst src = foldr f dst indexedRows
  where
    indexedRows = zip [r..] . map (zip [c..]) . toLists $ src
    f (r', indexedCols) dst' = foldr (g r') dst' indexedCols
    g r' (c', x) dst' = fromMaybe dst' $ safeSet x (r', c') dst'

setMinorMatrix :: forall a. (Int, Int) -> Matrix a -> Matrix a -> Matrix a
setMinorMatrix (r, c) dst src = sequenceA inserted
  & fromMaybe (error "unpossible!")
  where
    inserted = foldr copyCol m' indexedCol
      where
        m' = foldr copyRow adjusted indexedRow

        indexedCol = zip [1..] . V.toList $ dst ^. col c
        indexedRow = zip [1..] . V.toList $ dst ^. row r

        copyRow (c', x) = elemAt (r, c') .~ Just x
        copyCol (r', x) = elemAt (r', c) .~ Just x

    adjusted = injected ^. slidingRows (nrows mm + 1) r
                         . slidingCols (ncols mm + 1) c

    injected = extendTo Nothing (nrows mm + 1) (ncols mm + 1) mm

    mm = Just <$> src

setDiag :: Foldable t => Matrix a -> t a -> Matrix a
setDiag m xs = foldr f m . zip [1..] . F.toList $ xs
  where
    f (n, x) m' = m' & elemAt (n, n) .~ x

slideRows :: Int -> Int -> Matrix a -> Matrix a
slideRows s d m
  | s > d     = slideRows (s - 1) d $ m ^. switchingRows s (s - 1)
  | s < d     = slideRows (s + 1) d $ m ^. switchingRows s (s + 1)
  | otherwise = m

slideCols :: Int -> Int -> Matrix a -> Matrix a
slideCols s d m
  | s > d     = slideCols (s - 1) d $ m ^. switchingCols s (s - 1)
  | s < d     = slideCols (s + 1) d $ m ^. switchingCols s (s + 1)
  | otherwise = m
