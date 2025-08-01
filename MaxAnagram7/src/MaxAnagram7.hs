module MaxAnagram7 ( maxAnagram7 )
    where
import Data.Digits
import Data.Sort

maxAnagram7 :: Integer -> Integer
maxAnagram7 n = case findMaxAnagram7 (maxAnagram n) of
                  Just a -> a
                  Nothing -> -1

findMaxAnagram7 :: Integer -> Maybe Integer
findMaxAnagram7 a = if (mod a 7) == 0 
                       then Just a
                       else case nextAnagram a of
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
    let digits = fromNumber n
        suffix = descending digits
    let prefix = n `div` 100
        suffix = n `mod` 100
        unity = n `mod` 10
        newSuffix = n `div` 10
    in case nextAnagram suffix of
         Just nextSuffix -> Just $ prefix * 100 + nextSuffix
         Nothing -> Just $ unity * 100 + maxAnagram newSuffix

descending :: [Integer] -> [Integer] -> [Integer]
descending [] result = result
descending [x] result = x:result
descending (x:y:xs) result | y > x = x:result
descending (x:y:xs) result = descending (y:xs) x:result

removeNext :: [Integer] -> Integer -> (Integer,[Integer]) -> (Integer,[Integer])
removeNext [] _ (r,ys) = (r, ys)
removeNext (x:xs) p (r,ys) | x >= p = removeNext xs p (r,x:ys)
removeNext (x:xs) p (r,ys) | x <= r = removeNext xs p (r,x:ys)
removeNext (x:xs) p (r,ys) | x > r  = removeNext xs p (x,r:ys)


nextAnagram 8740 = Just 8470

fromNumber :: Integer -> [Integer]
fromNumber = digits 10

toNumber :: [Integer] -> Integer
toNumber = unDigits 10

-- 8740
-- 8740  0 < 4 → 87↔04 → 8704
-- 8704  4,0 0 < 7 8↔ max(swap [4,0] 7) → 8470


