
import ExprT
import Parser
import Data.Maybe

----------- Exercise 1 ---------------------------------------------------
--------------------------------------------------------------------------

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Mul x y) = (eval x) * (eval y)
eval (Add x y) = (eval x) + (eval y)

----------- Exercise 2 ---------------------------------------------------
--------------------------------------------------------------------------

evalStr :: String -> Maybe Integer
evalStr s
	| isExprT (toExprT s) = Just $ eval (fromJust (toExprT s))
	| otherwise = Nothing

toExprT :: String -> Maybe ExprT
toExprT s = parseExp Lit Add Mul s

isExprT :: Maybe a -> Bool
isExprT Nothing = False
isExprT (Just a) = True

----------- Exercise 3 ---------------------------------------------------
--------------------------------------------------------------------------

class Expr a where
	lit :: Integer -> a
	mul :: a -> a -> a
	add :: a -> a -> a

instance Expr ExprT where
	lit a = (Lit a)
	mul x y = Mul x y
	add x y = Add x y

instance Expr Integer where
	lit a = a
	mul x y = x * y
	add x y = x + y

instance Expr Bool where
	lit a
		| (a <= 0) = False
		| otherwise = True
	mul x y = (x && y)
	add x y = (x || y)

newtype MinMax  = MinMax {getMMVal :: Integer} deriving (Eq, Show)
newtype Mod7    = Mod7 {getMODVal :: Integer} deriving (Eq, Show)

instance Expr MinMax where
	lit a = MinMax a
	mul x y
		| ((getMMVal x) < (getMMVal y)) = x 
		| otherwise = y
	add x y
		| ((getMMVal x) < (getMMVal y)) = y
		| otherwise = x

instance Expr Mod7 where
	lit a = Mod7 a
	mul x y = Mod7 $ ((getMODVal x) * (getMODVal y)) `mod` 7
	add x y = Mod7 $ ((getMODVal x) + (getMODVal y)) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 5"