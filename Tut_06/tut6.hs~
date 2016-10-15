-- crossroad shortest path problem
data Section = Section {
    getA :: Double,
    getB :: Double,
    getC :: Double,
    getD :: Double
} deriving (Show)

type RoadSystem = [Section]

data Label = A | B | C | D deriving (Show)

type Path = [(Label, Double)]

l2h :: RoadSystem
l2h = [Section 0.084 0.0064 0.027 0.0072,
       Section 0.00588 0.01512 0.00108 0.00972
      ]

roadStep :: (Path,Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c d) = 
    let lengthA = sum (map snd $ pathA)   --snd -> second component
        lengthB = sum (map snd $ pathB)
        t2t = lengthA + a
        b2t = lengthB + c
        t2b = lengthA + b
        b2b = lengthB + d
        new2Top = if t2t >= b2t then (A, a) : pathA else (C,c):pathB
        new2Bot = if b2b >= t2b then (D, d) : pathA else (B,b):pathA
    in (new2Top, new2Bot)

solve x = foldl roadStep ([],[]) x

-- INPUT:
-- observation space
-- state space
-- sequence of observations
-- transition matrix
-- emission matrix
-- initial probabilities
--
-- OUTPUT:
-- most likely hidden state sequence
-- viterbi o s y a b p =
    -- Build sections


-- viterbi ["HEALTHY", "FEVER"] ["NORMAL","COLD","DIZZY"] [0.6, 0.4] [0.7, 0.3] [0.5, 0.4, 0.1] [0.1, 0.3, 0.6]
    
