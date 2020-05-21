{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Matrix.Lens
  ( flattened
  , col
  , cols
  , determinant
  , diag
  , elemAt
  , getDeterminant
  , getSize
  , inverted
  , isSquare
  , minor
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

import           Control.Lens        hiding ( set )
import           Data.Bifunctor             ( first )
import qualified Data.Foldable  as F        ( toList )
import qualified Data.List      as L
import           Data.Matrix
import           Data.Maybe                 ( fromMaybe )
import           Data.Vector                ( Vector )
import qualified Data.Vector    as V

elemAt :: (Int, Int) -> Lens' (Matrix a) a
elemAt (i, j) = lens (getElem i j) (\m x -> setElem x (i, j) m)

row :: Int -> Lens' (Matrix a) [a]
row r = lens (V.toList . getRow r) (\m -> setRow r m . V.fromList)

col :: Int -> Lens' (Matrix a) [a]
col c = lens (V.toList . getCol c) (\m -> setCol c m . V.fromList)

transposed :: Iso' (Matrix a) (Matrix a)
transposed = iso transpose transpose

scaled :: Num a => a -> Iso' (Matrix a) (Matrix a)
scaled n = iso (scaleMatrix n) (scaleMatrix . negate $ n)

rows :: Lens' (Matrix a) [[a]]
rows = lens toLists (const fromLists)

cols :: Lens' (Matrix a) [[a]]
cols = lens (L.transpose . toLists) (const (fromLists . L.transpose))

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

flattened :: Traversal' (Matrix a) a
flattened = rows . each . each

sub :: (Int, Int) -> (Int, Int) -> Lens' (Matrix a) (Matrix a)
sub (r1, c1) (r2, c2) = lens (submatrix r1 r2 c1 c2) (setSubMatrix (r1, c1))

minor :: (Int, Int) -> Lens' (Matrix a) (Matrix a)
minor (r, c) = lens (minorMatrix r c) (setMinorMatrix (r, c))

inverted :: (Eq a, Fractional a) => Prism' (Matrix a) (Matrix a)
inverted = flip prism (\x -> first (const x) . inverse $ x) $ \x -> case inverse x of
  Left  _ -> x
  Right y -> y

diag :: Lens' (Matrix a) [a]
diag = lens (V.toList . getDiag) (\m -> setDiag m . V.fromList)

-- ================================================================ --

--   :: Getter (Matrix a) (Int, Int)
size :: Optic' (->) (Const (Int, Int)) (Matrix a) (Int, Int)
size = to getSize

getSize :: Matrix a -> (Int, Int)
getSize m = (nrows m, ncols m)

--          :: Getter (Matrix a) (Maybe a)
determinant :: (Contravariant f, Num a, Profunctor p)
            => p (Maybe a) (f (Maybe a))
            -> p (Matrix a) (f (Matrix a))
determinant = to getDeterminant

getDeterminant :: Num a => Matrix a -> Maybe a
getDeterminant m
  | m ^. size == (2, 2) = Just . twoByTwoDet $ m
  | otherwise           = laplace m

twoByTwoDet :: Num a => Matrix a -> a
twoByTwoDet m = case map ((m ^.) . elemAt) coords of
  [a, b, c, d] -> a * d - b * c
  _other       -> error "unpossible! (2)"
  where
    coords =
      [ (1, 1)
      , (1, 2)
      , (2, 1)
      , (2, 2)
      ]

laplace :: Num a => Matrix a -> Maybe a
laplace m
  | not . isSquare $ m = Nothing
  | otherwise = Just . sum . zipWith (*) (cycle [1, -1]) . map f $ [1..ncols m]
  where
    r   = 1
    f c = ((e *) <$> getDeterminant (m ^. minor pair))
            & fromMaybe (error "unpossible! (3)")
      where
        e    = m ^. elemAt pair
        pair = (r, c)

isSquare :: Matrix a -> Bool
isSquare = uncurry (==) . getSize

-- ================================================================ --

setRow :: Int -> Matrix a -> Vector a -> Matrix a
setRow r m v = foldr (\(c, x) -> setElem x (r, c)) m $
  zip [1..] (V.toList v)

setCol :: Int -> Matrix a -> Vector a -> Matrix a
setCol c m v = foldr (\(r, x) -> setElem x (r, c)) m $
  zip [1..] (V.toList v)

setSubMatrix :: (Int, Int) -> Matrix a -> Matrix a -> Matrix a
setSubMatrix (r, c) dst src = foldr f dst indexedRows
  where
    indexedRows = zip [r..] . map (zip [c..]) . toLists $ src
    f (r', indexedCols) dst' = foldr (g r') dst' indexedCols
    g r' (c', x) dst' = fromMaybe dst' $ safeSet x (r', c') dst'

setMinorMatrix :: forall a. (Int, Int) -> Matrix a -> Matrix a -> Matrix a
setMinorMatrix (r, c) dst src = sequenceA inserted
  & fromMaybe (error "unpossible! (1)")
  where
    inserted = foldr copyCol m' indexedCol
      where
        m' = foldr copyRow adjusted indexedRow

        indexedCol = zip [1..] $ dst ^. col c
        indexedRow = zip [1..] $ dst ^. row r

        copyRow (c', x) = elemAt (r, c') ?~ x
        copyCol (r', x) = elemAt (r', c) ?~ x

    adjusted = injected ^. slidingRows (nrows mm + 1) r
                         . slidingCols (ncols mm + 1) c

    injected = extendTo Nothing (nrows mm + 1) (ncols mm + 1) mm

    mm = Just <$> src

setDiag :: Foldable t => Matrix a -> t a -> Matrix a
setDiag m = foldr f m . zip [1..] . F.toList
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
