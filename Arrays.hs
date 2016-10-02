import Data.Array
-- Arrays

-- an array of all the fibbonacci numbers from 0 to n. 
fibs    :: Int -> Array Int Int
fibs n  = a where a = array (0,n) 
                        ([(0,1), (1,1)] ++                          -- Inital values
                         [(i, a!(i-2) + a!(i-1)) | i <- [2..n]])    -- Fib following first 2

twoDExample1    :: Int -> Array (Int, Int) Int
twoDExample1 n  = a where
            a = array ((1,1), (n,n)) ([ ((i,j),2) | i <- [1..n], j <- [1..n]])

twoDExample2    :: Int -> Array (Int, Int) Int
twoDExample2 n  = a where
            a = array ((0,0), (n,n)) ([ ((i,j),2) | i <- [0..n], j <- [0..n]])

-- a 2x2 matrix in which element of first row and column all have value 1 and other elements are sums of their
-- neighbours to the west, northwest and north.
wavefront    :: Int -> Array (Int, Int) Int
wavefront n  = a where
                a = array ((1,1),(n,n)) 
                    ([((1,j), 1) | j <- [1..n]] ++  -- first row
                     [((i,1), 1) | i <- [1..n]] ++  -- first column
                     [((i,j), (a!(i,j-1) + a!(i-1,j) + a!(i-1,j-1)) ) | i <- [2..n], j <- [2..n]])   -- from (2,2) -> (n,n)
