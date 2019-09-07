module RLE.Utils where

import Data.List

-- groups similar elements
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (== x) xs) : pack (dropWhile (== x) xs)

-- a few RLE utils
rleCompress :: Eq a => [a] -> [(a, Int)]
rleCompress [] = []
rleCompress y@(x:xs) =
  (x, length $ head $ pack y) : rleCompress (dropWhile (== x) xs)

rleReduce :: (Eq a, Ord a) => [(a, Int)] -> [(a, Int)]
rleReduce [] = []
rleReduce a = filter appears $ map reducer $ groupBy alike $ sortOn fst a
  where
    appears x = snd x /= 0
    alike x y = fst x == fst y
    reducer w = (fst $ head w, sum $ map snd w)

rleExpandPiece :: (a, Int) -> [a]
rleExpandPiece (a, n) = map (const a) [1 .. n]

rleExpand :: [(a, Int)] -> [a]
rleExpand = concatMap rleExpandPiece

rleAddToList :: (Eq a, Ord a) => [a] -> [(a, Int)] -> [(a, Int)]
rleAddToList ls x = rleReduce $ rleCompress (ls ++ rleExpand x)
