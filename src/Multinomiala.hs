module Multinomiala where

import           Data.List hiding (foldl, foldr, map)
import           Data.Set  hiding (foldl, foldr, map)

data Mono =
  Mono
    { coef :: Integer
    , pw   :: Int
    }
  deriving (Eq, Ord)

monoProd :: Mono -> Mono -> Mono
monoProd (Mono c e) (Mono c' e') = Mono (c * c') (e + e')

instance Semigroup Mono where
  (<>) = monoProd

instance Monoid Mono where
  mempty = Mono 1 0

instance Show Mono where
  show (Mono c e) = show c ++ "x^" ++ show e

newtype Poly =
  Poly [Mono]

instance Show Poly where
  show (Poly [])  = ""
  show (Poly [x]) = show x
  show (Poly xs)  = intercalate "+" $ map show xs

boolToInt :: Bool -> Integer
boolToInt True  = 1
boolToInt False = 0

mSumTwo :: Mono -> Mono -> Mono
mSumTwo m n = Mono (coef n + boolToInt (pw m == pw n) * coef m) (pw n)

-- sum can be done folding on two sides
mSumr :: [Mono] -> Mono
mSumr []     = Mono 0 0
mSumr [x]    = x
mSumr (x:xs) = foldr mSumTwo x xs

mSuml :: [Mono] -> Mono
mSuml []     = Mono 0 0
mSuml [x]    = x
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

pSum :: [[Mono]] -> [Mono]
pSum []     = []
pSum [x]    = x
pSum (x:xs) = foldr pSuml x xs -- from testing, we know they are equal

mProd :: Mono -> Mono -> Mono
mProd (Mono c e) (Mono c' e') = Mono (c * c') (e + e')

mRaise :: Mono -> Int -> Mono
mRaise m exp = Mono ((coef m) ^ exp) ((pw m) * exp)

pProd :: Poly -> Poly -> Poly
pProd (Poly []) _         = Poly []
pProd _ (Poly [])         = Poly []
pProd (Poly ps) (Poly qs) = Poly $ concat [[u `monoProd` v | u <- ps] | v <- qs]
