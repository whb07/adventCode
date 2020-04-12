module Lib  (
    fuelNeeded,
    totalFuel,
    readValue,
    operationValues,
    insertValue,
    performOperation,
    processAllOps
) where 

fuelNeeded :: Integer -> Integer
fuelNeeded n
    | z >= 2 = z - 2
    | otherwise = 0
    where z = div n 3

totalFuel :: Integer -> Integer
totalFuel n
    | n <= 0 = 0
    | otherwise = x + totalFuel x
        where x = fuelNeeded n


operationValues :: [Int] -> Int -> Int -> [Int]
operationValues lst t d = drop d (take t lst)



data Result = Valid Int Int | Invalid deriving Show

performOperation :: [Int] -> [Int] -> Result
performOperation (a: b: c: d: _) lst
    | a == 1 = Valid (readValue lst b + readValue lst c) d 
    | a == 2 = Valid (readValue lst b * readValue lst c) d 
    | otherwise = Invalid
performOperation _ _= Invalid


insertValue :: Result -> [Int] -> [Int]
insertValue (Valid val idx) lst =
    take idx lst  ++ [val] ++ drop (idx + 1) lst

insertValue Invalid lst = lst
    

readValue :: [Int] -> Int -> Int
readValue lst n = 
    if n < length lst then lst !! n else -1


processAllOps :: [Int] -> Int -> Int -> [Int]
processAllOps lst t d =
    case v of
        Invalid -> lst
        _ -> processAllOps (insertValue v lst) (t + 4) (d + 4)
    where v = performOperation (operationValues lst t d) lst;


