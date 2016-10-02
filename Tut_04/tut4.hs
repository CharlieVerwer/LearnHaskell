----------- EXERSIZE 1 ---------------------------------------------------
--------------------------------------------------------------------------

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
	| even x    = (x - 2) * fun1 xs
	| otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldl (\x y -> x * (y-2)) 1 (filter (even) xs)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
	| even n = n + fun2 (n `div` 2)
	| otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' n = foldl (\x y -> if (even y) then (x + y) else x) 0 
			(takeWhile (>1) 
				(iterate (\x -> if (even x) then (x `div` 2) else (3 * x + 1)) 
					n))

----------- EXERCISE 2 ---------------------------------------------------
--------------------------------------------------------------------------

data Tree a = Leaf
		| Node Integer (Tree a) a (Tree a)
	deriving (Show, Eq)

leaf x = Node 0 Leaf x Leaf

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree xs = foldr (insert) Leaf xs

insert :: t -> Tree t -> Tree t
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node height left val right)
	| (getHeight left) < (getHeight right) = Node height (insert x left) val right
	| (getHeight left) > (getHeight right) = Node height left val (insert x right)
	| otherwise = Node (rightHeight + 1) left val (insert x right)
	where rightHeight = getHeight (insert x right)

getHeight :: Tree t -> Integer
getHeight Leaf = -1
getHeight (Node i _ _ _) = i


