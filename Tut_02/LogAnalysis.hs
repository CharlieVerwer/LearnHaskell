-- {-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

----------- Parse an individual message ----------------------------------
--------------------------------------------------------------------------

-- parseMessage "E 2 562 help help"
--  == LogMessage (Error 2) 562 "help help"

-- parseMessage "I 29 la la la"
--  == LogMessage Info 29 "la la la"

-- parseMessage "This is not in the right format"
--  == Unknown "This is not in the right format"

parseMessage :: String -> LogMessage
parseMessage [] = Unknown "This is not in the right format"
parseMessage (t:m)
	| t == 'I' = parseInfoMessage (words (tail m))
	| t == 'W' = parseWarningMessage (words (tail m))
	| t == 'E' = parseErrorMessage (words (tail m))
	| otherwise = Unknown "This is not in the right format"

parseInfoMessage :: [String] -> LogMessage
parseInfoMessage ws = LogMessage 
								Info 
								(read (head ws) :: Int) 
								(unwords (tail ws))

parseWarningMessage :: [String] -> LogMessage
parseWarningMessage ws = LogMessage
								Warning 
								(read (head ws) :: Int) 
								(unwords (tail ws))

parseErrorMessage :: [String] -> LogMessage
parseErrorMessage ws = LogMessage
								(Error (read (head ws) :: Int))
								(read (head (tail ws)) :: Int)
								(unwords (tail (tail ws)))


----------- Parse an entire file -----------------------------------------
--------------------------------------------------------------------------

-- TO TEST
--		testParse parse 10 "error.log"

parse :: String -> [LogMessage]
parse [] = []
parse file = parseMessage (head (lines file)) : parse (unlines (tail (lines file)))


----------- Inserting into a message tree --------------------------------
--------------------------------------------------------------------------

-- Message type is unknown
-- Tree is a leaf
-- add to left
-- add to right
insert :: LogMessage -> MessageTree -> MessageTree
insert lm mt
		| isUnknown lm = mt
		| isLeaf mt = Node (Leaf) lm (Leaf)
		| (timeStamp lm) < (timeStamp (getNode mt)) = Node (insert (lm) (leftTree mt)) (getNode mt) (rightTree mt)
		| (timeStamp lm) > (timeStamp (getNode mt)) = Node (leftTree mt) (getNode mt) (insert (lm) (rightTree mt))
		| otherwise = Node (insert (lm) (leftTree mt)) (getNode mt) (rightTree mt)

isUnknown :: LogMessage -> Bool
isUnknown (Unknown _) 	= True
isUnknown _ 			= False

isLeaf :: MessageTree -> Bool
isLeaf (Leaf)	 		= True
isLeaf _				= False

leftTree :: MessageTree -> MessageTree
leftTree (Node left _ _) = left

getNode :: MessageTree -> LogMessage
getNode (Node _ mid _) = mid

rightTree :: MessageTree -> MessageTree
rightTree (Node _ _ right) = right

timeStamp :: LogMessage -> TimeStamp
timeStamp (LogMessage _ timestamp _) = timestamp


----------- Building a message tree --------------------------------------
--------------------------------------------------------------------------

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (lm:lms) = insert lm (build lms)

----------- Printing a message tree in order traversal -------------------
--------------------------------------------------------------------------

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder mt = (inOrder (leftTree mt)) ++ [getNode(mt)] ++ (inOrder (rightTree mt))

----------- Only get error messages with a severity of 50 or above -------
--------------------------------------------------------------------------

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms = toStrings (onlySevereErrors (onlyErrors (inOrder (build lms))))

-- Filter only severe errors (ASSUMES ONLY ERRORS)
onlySevereErrors :: [LogMessage] -> [LogMessage]
onlySevereErrors lms
	| lms == [] = []
	| getSeverity (messageType (head lms)) < 50 = onlySevereErrors(tail lms)
	| getSeverity (messageType (head lms)) > 50 = (head lms) : onlySevereErrors(tail lms)

getSeverity (Error s) = s 

-- Filter to only error messages
onlyErrors :: [LogMessage] -> [LogMessage]
onlyErrors lms
	| lms == [] = []
	| isError (messageType (head lms)) = (head lms) : onlyErrors (tail lms)
	| otherwise = onlyErrors (tail lms)

isError :: MessageType -> Bool
isError (Error _) = True
isError _ = False

-- get message type
messageType :: LogMessage -> MessageType
messageType (LogMessage mt _ _) = mt

-- get String
getString :: LogMessage -> String
getString (LogMessage _ _ s) = s

toStrings :: [LogMessage] -> [String]
toStrings lms
	| lms == [] = []
	| otherwise = (getString (head lms)) : toStrings(tail lms)

-- TO TEST IT ALL
-- testWhatWentWrong parse whatWentWrong "sample.log"