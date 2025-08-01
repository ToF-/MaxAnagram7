module MaxAnagram7Spec ( spec )
    where

import MaxAnagram7
import Test.Hspec

spec :: SpecWith ()
spec = do
    describe "simple acceptance check" $ do
        it "should make 8740 the maximum anagram of 4807 that is a multiple of 7" $ do
            maxAnagram7 4807 `shouldBe` (8470 :: Integer)
            mod 8470 7 `shouldBe` (0 :: Integer)
    
--     describe "non trivial acceptance check" $ do
--         it "should find the largest anagram of 7^100" $ do
--             let n = product (take 100 (repeat 7)) :: Integer
-- 
--             let m = 999999999999999999999999999999999999999999999999999999999999999999999999988888888888888888888888888888888888888888888888888888888888888888888888888888888888777777777777777777777777777777777777777777777777777777777777777777777776666666666666666666666666666666666666666666666666666666666666666666666666666666666555555555555555555555555555555555555555555555555555555555555555555555555555555555555554444444444444444444444444444444444444444444444444444444444444444444444444444444333333333333333333333333333333333333333333333333333333333333333333333333333333333333333322222222222222222222222222222222222222222222222222222222222222222222222222222222222222211111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111101000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 :: Integer
-- 
--             maxAnagram7 n `shouldBe` m 

        it "should succeed with a trivial case of no anagram" $ do
            maxAnagram7 8 `shouldBe` -1

        it "should succeed with a trivial case" $ do
            maxAnagram7 7 `shouldBe` 7

        it "should succeed with a 2 digit number" $ do
            maxAnagram7 12 `shouldBe` 21
            maxAnagram7 82 `shouldBe` 28
            maxAnagram7 59 `shouldBe` -1

        it "should succeed with a 3 digit number" $ do
            maxAnagram7 777 `shouldBe` 777
            maxAnagram7 940 `shouldBe` 490
            maxAnagram7 417 `shouldBe` 714
            maxAnagram7 287 `shouldBe` 728
