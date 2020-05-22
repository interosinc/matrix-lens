{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Matrix.Lens.Oof
  ( determinant
  , getSize
  , size
  ) where

import Prelude

import Control.Lens              ( Getter
                                 , to
                                 )
import Data.Matrix               ( Matrix )
import Data.Matrix.Lens.Internal ( getDeterminant
                                 , getSize
                                 )

determinant :: Num a => Getter (Matrix a) (Maybe a)
determinant = to getDeterminant

size :: Getter (Matrix a) (Int, Int)
size = to getSize
