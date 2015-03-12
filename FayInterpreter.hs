module FayInterpreter where

data Room = Room { roomId :: Int, desc :: String, commandList :: [String], commandVals :: [Int]} deriving Show
data StringCarrier = StringCarrier { thisValue :: String } deriving Show

returnTuple :: [(Int, Int)]
returnTuple = [(1, 1), (2, 2), (3, 3)]

roomMake :: Int -> StringCarrier -> [StringCarrier] -> [Int] -> Room
roomMake w x y z = Room w (thisValue x) (stringArrayMake y []) z

stringArrayMake :: [StringCarrier] -> [String] -> [String]
stringArrayMake x y
   | length x == 0 = y
   | otherwise = stringArrayMake (tail x) ([thisValue (head x)] ++ y)

insertCommand :: Room -> StringCarrier -> Int -> Room
insertCommand x y z = finalRoom
   where finalRoom = Room (roomId x) (desc x) (commandList x ++ [thisValue y]) (commandVals x ++ [z])

compareCommand :: Room -> StringCarrier -> Int
compareCommand x y = compareCommandIterator (commandList x) (commandVals x) y 0

compareCommandIterator :: [String] -> [Int] -> StringCarrier -> Int -> Int
compareCommandIterator c v s i
   | length c == i = -1
   | (thisValue s) == (c !! i) = v !! i
   | otherwise = compareCommandIterator c v s (i + 1)