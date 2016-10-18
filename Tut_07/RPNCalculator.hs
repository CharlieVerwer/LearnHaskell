import Data.List  
import Data.Either  
import Control.Applicative  
  
solveRPN :: String -> Either String Float  
solveRPN = head . foldl foldingFunction [] . words  
    
foldingFunction :: [Either String Float] -> String -> [Either String Float]
foldingFunction [] _ = [Left "Too few arguments!"]
foldingFunction (Right x:Right y:ys) "*" = ((*) <$> Right x <*> Right y):ys  
foldingFunction (Right x:Right y:ys) "+" = ((+) <$> Right x <*> Right y):ys  
foldingFunction (Right x:Right y:ys) "-" = ((-) <$> Right y <*> Right x):ys  
foldingFunction (Right x:Right y:ys) "/" = ((/) <$> Right y <*> (validDivisor $ Right x)):ys  
foldingFunction (Right x:Right y:ys) "^" = ((**) <$> Right y <*> Right x):ys  
foldingFunction (Right x:xs) "ln" = ((log) <$> (validLog $ Right x)):xs  
foldingFunction xs "sum" = [Right (sum $ rights xs)]  
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
