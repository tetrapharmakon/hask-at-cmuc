module Multinomiala where

import Data.List hiding (foldl, foldr, map)
import Data.Set hiding (foldl, foldr, map)

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

-- sum can be done folding on two sides
mSumr :: [Mono] -> Mono
mSumr [] = Mono 0 0
mSumr [x] = x
mSumr (x:xs) = foldr mSumTwo x xs

mSuml :: [Mono] -> Mono
mSuml [] = Mono 0 0
mSuml [x] = x
mSuml (x:xs) = foldl mSumTwo x xs

-- sum can be done folding on two sides  
pSuml :: [Mono] -> [Mono] -> [Mono]
pSuml p q = map mSuml $ groupBy samePow $ sortOn pw (p ++ q)
  where
    samePow x y = pw x == pw y

pSumr :: [Mono] -> [Mono] -> [Mono]
pSumr p q = map mSumr $ groupBy samePow $ sortOn pw (p ++ q)
  where
    samePow x y = pw x == pw y

mRaise :: Mono -> Int -> Mono
mRaise m exp = Mono (c ^ exp) (e * exp)
  where
    c = coef m
    e = pw m
