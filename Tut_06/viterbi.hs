import Data.List

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

-- Example 1 viterbi -----------------------------------------
states = ["Healthy", "Fever"]
observations = ["normal", "cold", "dizzy"]
start_probability = [0.6, 0.4]
-- states to states
transition_probability = [[0.7, 0.3],
                          [0.4, 0.6]]
-- states to observations
emission_probability = [[0.5, 0.4, 0.1],
                        [0.1, 0.3, 0.6]]                         
--------------------------------------------------------------

viterbi s o s_p trans emis = 
    foldl (\xs i -> xs ++ [states!!(maxIndex(plies!!i))]) [] [0..(length o) - 1]
    where 
        firstPly = buildFirstPly s_p emis
        plies = foldl (\xs i -> xs ++ [(nextPly (xs!!(i-1)) trans emis i)] ) [firstPly] [1..(length o) - 1]

-- foldl (\xs y -> xs ++ [y] ) [0] [1..10]
-- = [0,1,2,3,4,5,6,7,8,9,10]

answer = viterbi states observations start_probability transition_probability emission_probability

-- given initial probabilities and emission probabilities, calculates the first ply of the tree
buildFirstPly :: Num t => [t] -> [[t]] -> [t]
buildFirstPly initial emis =
    [(initial!!i) * (index emis i 0) | i <- [0..(length initial) - 1]]

-- calculates the next ply given the old ply and the current observation (its index)
nextPly :: (Ord t, Num t) => [t] -> [[t]] -> [[t]] -> Int -> [t]
nextPly currentPly trans emis obsNum =
    [(nodeVal currentPly trans emis i obsNum) | i <- [0..(length currentPly) - 1]  ]

-- calculates the value of a node, choosing the max value of all incoming lines
nodeVal :: (Ord a, Num a) => [a] -> [[a]] -> [[a]] -> Int -> Int -> a
nodeVal prevPly trans emis stateNum obsNum =
    maximum [(prevPly!!i) * (index trans i stateNum) * (index emis stateNum obsNum) | i <- [0..(length prevPly) - 1] ]

-- return list[x][y]
index :: [[a]] -> Int -> Int -> a
index list x y = (list!!x)!!y

-- return the index of the max element of a list
maxIndex :: Ord a => [a] -> Int
maxIndex list = 
    removeJust $ elemIndex (maximum list) list

removeJust :: Num t => Maybe t -> t
removeJust (Just x) = x
removeJust Nothing = 0