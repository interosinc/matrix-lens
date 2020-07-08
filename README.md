# matrix-lens

Optics for the `matrix` package.

![matrix lenses](https://raw.githubusercontent.com/interosinc/matrix-lens/master/matrix-lens.jpg)

> That’s how it is with people. Nobody cares how it works as long as it works.
>
> - Councillor Hamann

Sparked by this [reddit post](https://old.reddit.com/r/haskell/comments/gazovx/monthly_hask_anything_may_2020/fqtk9oh/).

## Examples

The examples below make use of the following three matrices:

``` haskell
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
```

Accessing individual elements:

``` haskell
λ> exampleNotSquare ^. elemAt (4, 3)
120
```

``` haskell
λ> exampleInt & elemAt (2, 2) *~ 10
┌          ┐
│  1  2  3 │
│  4 50  6 │
│  7  8  9 │
└          ┘
```

Accessing individual columns:

``` haskell
λ> exampleInt ^. col 1
[1,4,7]
```

``` haskell
λ> exampleInt & col 2 . each *~ 10
┌          ┐
│  1 20  3 │
│  4 50  6 │
│  7 80  9 │
└          ┘
```

Accessing individual rows:

``` haskell
λ> exampleInt ^. row 1
[1,2,3]
```

``` haskell
λ> exampleInt & row 2 . each *~ 100
┌             ┐
│   1   2   3 │
│ 400 500 600 │
│   7   8   9 │
└             ┘
```

Manipulating all columns as a list:

``` haskell
λ> exampleInt ^. cols
[[1,4,7],[2,5,8],[3,6,9]]
```

``` haskell
λ> exampleInt & cols %~ reverse
┌       ┐
│ 3 2 1 │
│ 6 5 4 │
│ 9 8 7 │
└       ┘
```

Accessing all rows as a list:

``` haskell
λ> exampleInt ^. rows
[[1,2,3],[4,5,6],[7,8,9]]
```

``` haskell
λ> exampleInt & rows %~ map reverse
┌       ┐
│ 3 2 1 │
│ 6 5 4 │
│ 9 8 7 │
└       ┘
```

``` haskell
λ> exampleInt & partsOf (dropping 1 (rows . each)) %~ reverse
┌       ┐
│ 1 2 3 │
│ 7 8 9 │
│ 4 5 6 │
└       ┘
```

In addition to the above there are also `switching` and `sliding` Isos for both
rows and columns which allow you to swap two arbitrary rows or columns or slide
a row or column through the matrix to a different row or column (moving all
intervening rows or columns over in the direction of the source row or column):

``` haskell
λ> exampleNotSquare ^. switchingRows 1 4
┌             ┐
│ 100 110 120 │
│  40  50  60 │
│  70  80  90 │
│  10  20  30 │
└             ┘
```

``` haskell
λ> exampleNotSquare ^. slidingRows 1 4
┌             ┐
│  40  50  60 │
│  70  80  90 │
│ 100 110 120 │
│  10  20  30 │
└             ┘
```

..and similary for `switchingCols` and `switchingRows`.

An Iso exists for accessing the matrix with a given row scaled:

``` haskell
λ> exampleInt ^. scalingRow 1 10
┌          ┐
│ 10 20 30 │
│  4  5  6 │
│  7  8  9 │
└          ┘

λ> exampleInt & scalingRow 1 10 . flattened  *~ 2
┌                ┐
│ -200 -400 -600 │
│    8   10   12 │
│   14   16   18 │
└                ┘
```

Any valid sub matrix can be accessed via the `sub` lens:

``` haskell
λ> exampleNotSquare ^. sub (2, 1) (3, 2)
┌         ┐
│  40  50 │
│  70  80 │
└         ┘

λ> exampleNotSquare & sub (2, 1) (3, 2) . rows %~ reverse
┌             ┐
│  10  20  30 │
│  70  80  60 │
│  40  50  90 │
│ 100 110 120 │
└             ┘
```

The transposition of the matrix can be accessed via the `transposed` Iso:

``` haskell
λ> exampleInt ^. transposed
┌       ┐
│ 1 4 7 │
│ 2 5 8 │
│ 3 6 9 │
└       ┘

λ> exampleInt & transposed . taking 4 flattened *~ 10
┌          ┐
│ 10 20  3 │
│ 40  5  6 │
│ 70  8  9 │
└          ┘
```

You can also traverse the `flattened` matrix:

``` haskell
λ> exampleInt ^.. flattened
[1,2,3,4,5,6,7,8,9]
```

which is more useful for making modifications:

``` haskell
λ> exampleInt & flattened . filtered even *~ 10
┌          ┐
│  1 20  3 │
│ 40  5 60 │
│  7 80  9 │
└          ┘
```

``` haskell
λ> exampleInt & dropping 4 flattened *~ 10
┌          ┐
│  1  2  3 │
│  4 50 60 │
│ 70 80 90 │
└          ┘
```

Or a resized copy of the matrix:

``` haskell
λ> exampleInt ^. resized 99 (5, 5)
┌                ┐
│  1  2  3 99 99 │
│  4  5  6 99 99 │
│  7  8  9 99 99 │
│ 99 99 99 99 99 │
│ 99 99 99 99 99 │
└                ┘

λ> exampleInt & resized 99 (5, 5) . diag %~ reverse
┌          ┐
│ 99  2  3 │
│  4 99  6 │
│  7  8  9 │
└          ┘
```

Accessing the diagonal:

``` haskell
λ> exampleInt ^. diag
[1,5,9]

λ> exampleInt & diag %~ reverse
┌       ┐
│ 9 2 3 │
│ 4 5 6 │
│ 7 8 1 │
└       ┘

λ> exampleInt & diag . each *~ 10
┌          ┐
│ 10  2  3 │
│  4 50  6 │
│  7  8 90 │
└          ┘
```

Accessing inverse matrix is possible via the `inverted` optic.  Since not all
matrices have inverses `inverted` is a prism:

``` haskell
λ> exampleInvertible ^? inverted
Just ┌             ┐
│ 0 % 1 1 % 5 │
│ 1 % 1 3 % 5 │
└             ┘

λ> exampleInvertible & inverted . flattened *~ 2
┌                   ┐
│ (-3) % 2    1 % 2 │
│    5 % 2    0 % 1 │
└                   ┘
```

Minor matrices can be accessed by specifying the (r, c) to be removed:

``` haskell
λ> exampleInt ^. minor (1, 2)
┌     ┐
│ 4 6 │
│ 7 9 │
└     ┘

λ> exampleInt & minor (1, 2) . flattened *~ 10
┌          ┐
│  1  2  3 │
│ 40  5 60 │
│ 70  8 90 │
└          ┘
```

An Iso exists for accessing a scaled version of a matrix:

``` haskell
λ> exampleInt ^. scaled 10
┌          ┐
│ 10 20 30 │
│ 40 50 60 │
│ 70 80 90 │
└          ┘

λ> exampleInt & minor (1, 1) . scaled 10 . flattened  +~ 1
┌                ┐
│    1    2    3 │
│    4 -510 -610 │
│    7 -810 -910 │
└                ┘
```

Getters for the matrix determinant and size are also provided:

``` haskell
λ> exampleInt ^. determinant
Just 0

λ> exampleInvertible ^. determinant
Just ((-5) % 1)

λ> exampleNotSquare ^. determinant
Nothing

λ> exampleInt ^. size
(3,3)
λ> exampleInvertible ^. size
(2,2)
λ> exampleNotSquare ^. size
(4,3)
```

In addition an `_AsMatrix` prism is provided that can be used to view a valid
matrix inside any instance of the `AsMatrix` class.  Two instances are provided
for `AsMatrix` - one for lists of lists and one for strings.  You can provide
additonal instances to gain the ability to use the `_AsMatrix` prism on them.

The existing instances work like so:

``` haskell
λ> ns = [[1..3], [4..6 :: Int]]
λ> ns
[[1,2,3],[4,5,6]]
λ> ns & _AsMatrix %~ (transpose :: Matrix Int -> Matrix Int)
[[1,4],[2,5],[3,6]]

λ> s = " 10 20 30 \n 40 50 60 "
λ> s ^? _AsMatrix :: Maybe (Matrix Int)
Just ┌          ┐
│ 10 20 30 │
│ 40 50 60 │
└          ┘
λ> s & _AsMatrix %~ (transpose :: Matrix Int -> Matrix Int)
"\9484       \9488\n\9474 10 40 \9474\n\9474 20 50 \9474\n\9474 30 60 \9474\n\9492       \9496"
λ> s' = s & _AsMatrix %~ (transpose :: Matrix Int -> Matrix Int)
λ> putStrLn s'
┌       ┐
│ 10 40 │
│ 20 50 │
│ 30 60 │
└       ┘

λ> t = show (fromLists [[1..3],[4..6 :: Int]]) :: String
λ> putStrLn t
┌       ┐
│ 1 2 3 │
│ 4 5 6 │
└       ┘
λ> t' = t & _AsMatrix %~ (transpose :: Matrix Int -> Matrix Int)
λ> putStrLn t'
┌     ┐
│ 1 4 │
│ 2 5 │
│ 3 6 │
└     ┘
```

NB. The `_AsMatrix` definition for String will read matrices with or without
the unicode decorations on either side, but will always render them.
