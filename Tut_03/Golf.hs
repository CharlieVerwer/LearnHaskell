module Golf where

----------- HOPSCOTCH SKIPPING -------------------------------------------
--------------------------------------------------------------------------

skips :: [a] -> [[a]]
skips [] = []
skips xs = skips' xs 0 where
	skips' [] _ = [] 
	skips' xs n = [skip (xs) n] ++ skips' (tail xs) (n+1)

skip :: [a] -> Int -> [a]
skip [] _ = []
skip xs 0 = xs
skip xs n
	| length xs < (n+1) = [head xs]
	| otherwise = (head xs) : skip (iterate tail xs !! (n+1)) (n)

----------- LOCAL MAXIMA -------------------------------------------------
--------------------------------------------------------------------------

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima xs = localMaxima' xs 0 where
	localMaxima' [] _ = []
	localMaxima' xs n 
		| n >= length xs = []
		| isLocalMax xs n = xs !! n : localMaxima' xs (n+1)
		| otherwise = localMaxima' xs (n+1)

isLocalMax :: [Integer] -> Int -> Bool
isLocalMax [] _ = False
isLocalMax xs n
	| n < 1 = False
	| n >= (length xs - 1) = False
	| ((xs !! n) < (xs !! (n-1))) = False
	| ((xs !! n) < (xs !! (n+1))) = False
	| otherwise = True

----------- HISTOGRAM ----------------------------------------------------
--------------------------------------------------------------------------
-- to use: putStr (histogram [3,5])
-- putStr (histogram [1,4,5,4,6,6,3,4,2,0,4,9])
-- putStr (histogram [1,1,1,5])

-- maximum [1,2,4,3,5,2,2,2,1] = 5     		(maximum function is cool!)

-- Histogram of strictly numbers 1 to 9
histogram :: [Integer] -> String
histogram xs = '\n' : histogram' (frequencies xs) (maximum (frequencies xs)) ++ "\n"

-- histogram func that works off frequencies and a value to currently work at
histogram' :: [Integer] -> Integer -> String
histogram' [] _ = ""
histogram' xs n
	 | n <= 0 = "==========\n0123456789\n"
	 | otherwise = (currentLine (xs) (n)) ++ (histogram' (xs) (n-1))


-- goes through a list of frequencies, building a string of '*'s for values above
-- or equal to given roof, and ' ' for values below
currentLine :: [Integer] -> Integer -> String
currentLine [] _ = "\n"
currentLine _ 0 = ""
currentLine (x:xs) roof
	| x >= roof = '*' : currentLine (xs) (roof)
	| otherwise = ' ' : currentLine (xs) (roof)


-- frequencies [1,1,2,4,5,6,6,6] => [0,2,1,0,1,1,3,0,0,0]
frequencies :: [Integer] -> [Integer]
frequencies [] = [0,0,0,0,0,0,0,0,0,0]
frequencies (x:xs) = incrementNth (frequencies xs) (x)

-- increments nth item of a list
incrementNth :: [Integer] -> Integer -> [Integer]
incrementNth (x:xs) n
	| n == 0 = (x+1):xs
	| otherwise = x:incrementNth (xs) (n-1)

