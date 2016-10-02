
-- main = putStrLn "Hello, world!"

-- double a number
doubleMe x = x + x

-- double a number less than 100
doubleSmallNumber x = if x > 100
						then x
						else x*2

-- if statements are expression and so a value
doubleSmallNumber' x = (if x < 100 then x else x*2) + 1

-- lists
let lostNumbers = [4,8,15]

-- adding to end of a list runs through first bit
[1,2,3,4] ++ [9,10,11,12]

-- adding to start of a list is instant, also strings are lists of chars
'A':" SMALL CAT"

-- recommended to give functions type definitions
removeNonUppercase :: [Char] -> [Char] -- takes an array of chars and returns an array of chars
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

