
-- numbers first, operators after, apply that shit
-- Main> calcRPN "2 2 2 sum ** 2"
-- 2.0
-- *Main> calcRPN "1 2 3 + +"
-- 6.0
-- *Main> calcRPN "1 2 3 + -"
-- -4.0
-- *Main> calcRPN "1 2 3 - +"
-- 0.0
calcRPN :: String -> Double
calcRPN input = head (foldl foldF [] $ words input) where
    foldF (x:y:xs) "+" = (y+x):xs 
    foldF (x:y:xs) "*" = (y*x):xs 
    foldF (x:y:xs) "/" = (y/x):xs 
    foldF (x:y:xs) "-" = (y-x):xs 
    foldF (x:y:xs) "**" = (y**x):xs
    foldF xs "sum" = [sum xs] 
    foldF xs num = (read num):xs



-- crossroad shortest path problem
data Section = Section {
    getA :: Int,
    getB :: Int,
    getC :: Int
} deriving (Show)

type RoadSystem = [Section]

data Label = A | B | C deriving (Show)

type Path = [(Label, Int)]

-- A-50-.-05-.-40-.-10-.
--      30   20   25   0
-- B-10-.-90-.-02-.-08-.
l2h :: RoadSystem
l2h = [Section 50 10 30,
        Section 5 90 20,
        Section 40 2 25,
        Section 10 8 0]

roadStep :: (Path,Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) = 
    let lengthA = sum (map snd $ pathA)   --snd -> second component
        lengthB = sum (map snd $ pathB)
        f2A = lengthA + a
        c2A = lengthB + b + c
        f2B = lengthB + b
        c2B = lengthA + a + c        
        newA = if f2A <= c2A then (A, a) : pathA else (C,c):(B,b):pathB
        newB = if f2B <= c2B then (B, b) : pathA else (C,c):(A,a):pathA
    in (newA, newB)


solve x = foldl roadStep ([],[]) x

-- should be 30 not 20, seems to be working
-- *Main> let r1 = roadStep ([],[]) (Section 50 10 20)
-- *Main> r1
-- ([(C,20),(B,10)],[(B,10)])
-- *Main> let r2 = roadStep r1 (Section 5 90 20)
-- *Main> r2
-- ([(A,5),(C,20),(B,10)],[(C,20),(A,5),(C,20),(B,10)])
-- *Main> 
-- *Main> 

-- *Main> let r1 = roadStep ([],[]) (Section 50 10 30)
-- *Main> let r2 = roadStep r1 (Section 5 90 20)
-- *Main> r2
-- ([(A,5),(C,30),(B,10)],[(C,20),(A,5),(C,30),(B,10)])
-- *Main> 


