
data MaybeC a = NothingC | JustC Int a deriving (Show)

-- :info Functor
instance Functor MaybeC where
    fmap f NothingC = NothingC
    fmap f (JustC counter x) = JustC (counter+1) (f x)


t = JustC 0 "Hello"
-- fmap id t
-- => JustC 1 "Hello"

-- this shouldn't change, not a legal implementation 
-- of the fmap function as mapping identity over a thing
 -- shouldn't change anything

-- fmap (*2) $ fmap (+3) [1,2,3]
-- => [8,10,12]

-- fmap (\x -> (x+3)*2) [1,2,3]
-- => [8,10,12]

-----------------------
-- aplicative functors:

-- pure (+) <*> Just 5 <*> Just 7
-- (+) <$> Just 5 <*> Just 7
-- => Just 12

-- pure (++) <*> Just "jonta" <*> Just "volta"
-- (++) <$> Just "jonta" <*> Just "volta"
-- => Just "jontavolta"

-- (++) <$> Nothing <*> Just "volta"
-- => Nothing
-- so we can try apply functions to things and if any of them
-- are nothing, computation will fail.

-- fmap (++) (Just "jonta") <*> Just "volta"
-- => Just "jontavolta"

-- [(+)] <*> [1,2] <*> [3,4]
-- => [4,5,6,7]

-- pure (++) ["ha","heh","hmm"] <*> ["?","!,"."]
-- what... the... fuck... ?


