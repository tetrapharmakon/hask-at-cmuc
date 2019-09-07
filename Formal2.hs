module Formal where

import Data.List

newtype FPS v c =
  FPS
    { eval :: [v] -> c
    }

fpsSum :: (Num c) => FPS v c -> FPS v c -> FPS v c
fpsSum p q = FPS (\xs -> eval p xs + eval q xs)

facs :: [a] -> [([a], [a])]
facs xs = zip (inits xs) (tails xs)

fpsMult :: (Num c) => FPS v c -> FPS v c -> FPS v c
fpsMult p q = undefined

data Var =
  X

f :: FPS Var Int
f = FPS length

g :: FPS Var Int
g = FPS (const 2)
