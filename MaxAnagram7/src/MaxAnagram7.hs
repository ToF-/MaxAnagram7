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

nextAnagram :: Integer -> Maybe Integer
nextAnagram n | n < 10 = Nothing
nextAnagram n | n < 100 =
    let n' = toNumber $ reverse $ fromNumber n
     in if n' < n then Just n' else Nothing
nextAnagram n | n < 1000 =
    case nextAnagram (n `mod` 100) of
      Just s -> Just $ n `div` 100 + s
      Nothing -> Just $ (n `mod` 10) * 100 + maxAnagram ((n `div` 10) `mod` 10) + ((n `div` 100) `mod` 10)
nextAnagram 8740 = Just 8470

fromNumber = digits 10
toNumber = unDigits 10



