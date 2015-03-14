module FayInterpreter where

data Room = Room { roomId :: Int, desc :: String, commandList :: [String], commandVals :: [(Int, Int, Int)]} deriving Show
data StringCarrier = StringCarrier { thisValue :: String } deriving Show

--COMMAND VALUES

--first: command type
--second: associated room id (if applicable, otherwise -1)
--third: associated key value (if applicable, otherwise -1

--invalid command: first: -1

--command type 1: standard connection
--first: 0
--second: destination room id
--third: -1

--command type 2: locked connection
--first: 1
--second: destination room id
--third: required key value

--command type 3: key command
--first: 2
--second: key value to be acquired
--third: -1

--command type 4: upgrade key command
--first: 3
--second: key value required
--third: key value to upgrade to

returnTuple :: [(Int, Int)]
returnTuple = [(1, 1), (2, 2), (3, 3)]

returnTripleArray :: [(Int, Int, Int)]
returnTripleArray  = [(1, 1, 1), (2, 2, 2), (3, 3, 3)]

takeTriple :: (Int, Int, Int) -> Int
takeTriple (x, y, z) = x + y + z

roomMake :: Int -> StringCarrier -> [StringCarrier] -> [(Int, Int, Int)] -> Room
roomMake w x y z = Room w (thisValue x) (stringArrayMake y []) z

stringArrayMake :: [StringCarrier] -> [String] -> [String]
stringArrayMake x y
   | length x == 0 = y
   | otherwise = stringArrayMake (tail x) (y ++ [thisValue (head x)])
   
insertCommand :: Room -> StringCarrier -> (Int, Int, Int) -> Room
insertCommand x y z = finalRoom
   where finalRoom = Room (roomId x) (desc x) (commandList x ++ [thisValue y]) (commandVals x ++ [z])

compareCommand :: Room -> StringCarrier -> (Int, Int, Int)
compareCommand x y = compareCommandIterator (commandList x) (commandVals x) y 0

compareCommandIterator :: [String] -> [(Int, Int, Int)] -> StringCarrier -> Int -> (Int, Int, Int)
compareCommandIterator c v s i
   | length c == i = (-1, -1, -1)
   | (thisValue s) == (c !! i) = v !! i
   | otherwise = compareCommandIterator c v s (i + 1)