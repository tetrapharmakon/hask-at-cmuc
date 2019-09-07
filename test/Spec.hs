module Spec where

import Multinomiala
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

instance Arbitrary Mono where
  arbitrary = do
    c <- arbitrary
    Mono c <$> arbitrary

associative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associative f x y z = f (f x y) z == f x (f y z)

commutative :: Eq a => (a -> a -> a) -> a -> a -> Bool
commutative f x y = f x y == f y x

main :: IO ()
main =
  hspec $
  describe "Formal power series" $
    -- modifyMaxSuccess (const 500) $
   do
    it "monoProd has a neutral element" $
      property (\a -> monoProd (a :: Mono) (Mono 1 0) == a)
    it "monoProd is associative" $ property (associative monoProd)
    it "mSum is associative" $
      property (\a b c -> pSuml [a, b] [c] == pSumr [a, b] [c])
    it "monoProd is commutative" $ property (commutative monoProd)
