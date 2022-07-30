toDigits :: Integer -> [Integer]

toDigits n
    | (n < 1) = []
    | otherwise = (map (\x -> read [x] :: Integer) (show n))

toDigitsRev :: Integer -> [Integer]

toDigitsRev n
    | (n < 1) = []
    | otherwise = reverse (map (\x -> read [x] :: Integer) (show n))

doubleEveryOther :: [Integer] -> [Integer]

doubleEveryOther = reverse . zipWith (*) value . reverse
    where
    value = 1 : 2 : value

sumDigits :: [Integer] -> Integer
sumDigits list = foldl (\x y -> x + (if y > 9 then sumDigits (toDigits y) else y)) 0 list

checkRemainder :: Integer -> Bool
checkRemainder n = n `mod` 10 == 0

validate :: Integer -> Bool
validate = checkRemainder . sumDigits . doubleEveryOther . toDigits