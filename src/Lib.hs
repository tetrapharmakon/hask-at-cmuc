module Lib where

import Data.List hiding (foldr, map)
import Data.Set hiding (foldr, map)

data Mono =
  Mono
    { coef :: Integer
    , pw :: Int
    }
  deriving (Eq, Show, Ord)

monoProd :: Mono -> Mono -> Mono
monoProd (Mono c e) (Mono c' e') = Mono (c * c') (e + e')

instance Semigroup Mono where
  (<>) = monoProd

instance Monoid Mono where
  mempty = Mono 1 0

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
