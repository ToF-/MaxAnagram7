module MaxAnagram7 ( maxAnagram7 )
    where
import Data.Digits
import Data.Sort

maxAnagram7 :: Integer -> Integer
maxAnagram7 n = case findMaxAnagram7 (maxAnagram n) of
                  Just a -> a
                  Nothing -> -1

findMaxAnagram7 :: Integer -> Maybe Integer
findMaxAnagram7 a = case mod a 7 of
                      0 -> Just a
                      _ -> case nextAnagram a of
                             Just n -> findMaxAnagram7 n
                             Nothing -> Nothing

maxAnagram :: Integer -> Integer
maxAnagram n = unDigits 10 (sortBy (flip compare) (digits 10 n))
maxAnagram n | n < 10 = n
maxAnagram 12 = 21
maxAnagram 4807 = 8740

nextAnagram :: Integer -> Maybe Integer
nextAnagram n | n < 10 = Nothing
nextAnagram 8740 = Just 8470


