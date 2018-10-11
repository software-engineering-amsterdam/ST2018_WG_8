module Workshop6 where

import Lecture6

mapB :: (a -> b) -> Tree a -> Tree b
mapB f (T a []) = T (f a) []
mapB f (T a [x]) = T (f a) [ (mapB f x)]
