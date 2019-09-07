module Formal where

import Data.List

data FormalPowerSeries v c = FormalPowerSeries
    { 
        eval :: [v] -> c 
    }

formalSum :: (Num c) => FormalPowerSeries v c -> FormalPowerSeries v c -> FormalPowerSeries v c
formalSum p q = FormalPowerSeries (\ xs -> (eval p xs) + (eval q xs))
    
facs :: [a] -> [([a],[a])]
facs xs = zip (inits xs) (tails xs)

formalMult :: (Num c) => FormalPowerSeries v c -> FormalPowerSeries v c -> FormalPowerSeries v c
formalMult p q = FormalPowerSeries \ 

data Var = X

f :: FormalPowerSeries Var Int
f = FormalPowerSeries (length)

g :: FormalPowerSeries Var Int
g = FormalPowerSeries (\ _ -> 2)
