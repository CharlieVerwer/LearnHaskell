import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)


lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of 
        Nothing -> Left $ "Locker Number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken
            then Right code
            else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
    [(100, (Taken, "ZD39I")),
     (101, (Free, "JAH3I"))
    ]

-- *Main> lockerLookup 100 lockers
-- Left "Locker 100 is already taken!"

-- *Main> lockerLookup 101 lockers
-- Right "JAH3I"

-- *Main> lockerLookup 102 lockers
-- Left "Locker Number 102 doesn't exist!" 
