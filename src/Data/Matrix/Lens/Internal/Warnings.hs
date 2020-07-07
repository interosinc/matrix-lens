-- These functions are in a separate file so we can cover only these two
-- functions with the -Wno-redundant-constraints pragma and leave
-- -Wredundant-constraints enabled for everything else.  If anyone knows a
-- better way to handle this, please let me know.

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Matrix.Lens.Internal.Warnings
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
