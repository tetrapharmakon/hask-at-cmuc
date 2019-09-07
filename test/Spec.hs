module Spec where

import Lib
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

instance Arbitrary Mono where
  arbitrary = do
    c <- arbitrary
    Mono c <$> arbitrary

main :: IO ()
main =
  hspec $
  describe "Formal power series" $ do
    modifyMaxSuccess (const 10) $
      it "monoProd has a neutral element" $
      property (\a -> monoProd (a :: Mono) (Mono 1 0) == a)
