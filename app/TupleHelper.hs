module TupleHelper where

import Control.Monad.IO.Class

fst' :: (a, b, c) -> a
fst' (a, _, _) = a

last' :: (a, b, c) -> c
last' (_, _, c) = c
