-------------- Exercise 1 --------------------------------

toDigits	:: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = (toDigits (x `div` 10)) ++ [x `mod` 10]

toDigitsRev	:: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = (x `mod` 10) : (toDigitsRev (x `div` 10))

  -- f a b
  -- a `f` b

-------------- Exercise 2 --------------------------------

doubleEveryOtherFromLeft	:: [Integer] -> [Integer]
doubleEveryOtherFromLeft l
  | length l <= 1 = l
  | otherwise = [(l !! 0), (2 * (l !! 1))] ++ doubleEveryOther (tail (tail l))

doubleEveryOther 			:: [Integer] -> [Integer]	-- doubles every other from the right
doubleEveryOther l
  | length l <= 1 = l
  | otherwise = doubleEveryOther (init (init l)) ++ [2*(l !! ((length l) - 2)),(l !! ((length l) - 1))]

-------------- Exercise 3 ------------------------------

sumDigitsSingleNumber		:: Integer -> Integer
sumDigitsSingleNumber x
  | x < 10 = x
  | otherwise = (x `mod` 10) + sumDigitsSingleNumber (x `div` 10)

sumDigits 					:: [Integer] -> Integer
sumDigits l
  | l == [] = 0
  | otherwise = sumDigitsSingleNumber (l !! 0) + sumDigits (tail l)

-------------- Exercise 4 ------------------------------

validate :: Integer -> Bool
validate x = ((sumDigits (doubleEveryOther (toDigits (x)))) `mod` 10) == 0