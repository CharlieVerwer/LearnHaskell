import Data.List  
import Data.Either  
import Control.Applicative  
  
solveRPN :: String -> Either String Float  
solveRPN = head . foldl foldingFunction [] . words  
    
foldingFunction :: [Either String Float] -> String -> [Either String Float]
foldingFunction [] _ = [Left "Too few arguments!"]
foldingFunction (x:y:ys) "*" = (x >>= (\a -> y >>= (\b -> return (a * b)))) : ys
foldingFunction (x:y:ys) "+" = (x >>= (\a -> y >>= (\b -> return (a + b)))) : ys
foldingFunction (x:y:ys) "-" = (x >>= (\a -> y >>= (\b -> return (b - a)))) : ys
foldingFunction (x:y:ys) "/" = (validDivisor x >>= (\a -> y >>= (\b -> return (b / a)))) : ys
foldingFunction (x:y:ys) "^" = (x >>= (\a -> y >>= (\b -> return (b ** a)))) : ys 
foldingFunction (x:xs) "ln" = (validLog x >>= (\a -> return (log a))) : xs
foldingFunction xs "sum" = [sequence xs >>= (\y -> Right (sum y))]
foldingFunction xs numberString = Right (read numberString):xs

-- Checks if the given divisor =/= 0
validDivisor :: Either String Float -> Either String Float
validDivisor (Left _) = Left "Don't divide by zero!"
validDivisor (Right 0) = Left "Don't divide by zero!"
validDivisor (Right x) = Right x

-- Checks if the given number is greater than 0
validLog :: Either String Float -> Either String Float
validLog (Left _) = Left "Invalid Log function!"
validLog (Right x)
    | (x <= 0) = Left "Invalid Log function!"
    | otherwise = Right x