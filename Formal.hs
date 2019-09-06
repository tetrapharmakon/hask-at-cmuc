module Formal where

import Data.List hiding (foldr, map)
import Data.Set hiding (foldr, map)

data Mono =
  Mono
    { coef :: Integer
    , pw :: Int
    }
  deriving (Eq, Show, Ord)

-- newtype Poly =
--   Poly [Mono]
--   deriving (Eq, Show, Ord)
monoProd :: Mono -> Mono -> Mono
monoProd (Mono c e) (Mono c' e') = Mono (c * c') (e + e')

instance Semigroup Mono where
  (<>) = monoProd

instance Monoid Mono where
  mempty = Mono 1 0

-- seriesProd :: Poly -> Poly -> Poly
-- p `seriesProd` q =
--   [ sum [(p !! i) `monoProd` (q !! j) | i <- [1 ..], j <- [1 ..], i + j == n]
--   | n <- [1 ..]
--   ]
a = Mono 2 3 -- 2x^3

b = Mono 3 2 -- 3x^2

c = Mono 5 1 -- 5x

p = [a, b] -- 2x^3 + 3x^2

q = [a, c]

boolToInt :: Bool -> Integer
boolToInt True = 1
boolToInt False = 0

mSumTwo :: Mono -> Mono -> Mono
mSumTwo m n = Mono (coef n + boolToInt (pw m == pw n) * coef m) (pw n)

mSum :: [Mono] -> Mono
mSum [] = Mono 0 0
mSum [x] = x
mSum (x:xs) = foldr mSumTwo x xs

pSum :: [Mono] -> [Mono] -> [Mono]
pSum p q = map mSum $ groupBy samePow $ sortOn pw (p ++ q)
  where
    samePow x y = pw x == pw y
