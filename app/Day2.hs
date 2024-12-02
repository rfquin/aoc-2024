module Day2 where

day02 :: IO()
day02 = do
  input <- lines <$> readFile "inputs/day2-input.txt"
  putStrLn $ show $ solvePartTwo input

-- part two solution

solvePartTwo :: [String] -> Int
solvePartTwo s = foldl (+) 0 $ map transform $ map getRow s

transform :: [Int] -> Int
transform (xs)
 | (isAdjacent (xs)) || bruteforce (xs) = 1
 | otherwise = 0

bruteforce :: [Int] -> Bool
bruteforce xs = any (isAdjacent) (generate xs)

generate :: [Int] -> [[Int]]
generate (xs) = map (\y -> take y xs ++ drop (y+1) xs)[0..(length xs - 1)]

isAdjacent :: [Int] -> Bool
isAdjacent (xs) = isAdjacentDecreasing xs || isAdjacentIncreasing xs

findBadDecrease :: [Int] -> Int -> Maybe Int
findBadDecrease (x:x':xs) (a)
  | (x-x' >= 1) && (x-x' <= 3) = findBadDecrease (x':xs) (a+1)
  | otherwise = (Just a)
findBadDecrease (_) (_)= Nothing

findBadIncrease :: [Int] -> Int -> Maybe Int
findBadIncrease (x:x':xs) (a)
  | (x'-x >= 1) && (x'-x <= 3) = findBadIncrease (x':xs) (a+1)
  | otherwise = Just a
findBadIncrease (_) (_) = Nothing

-- part one solution

solvePartOne :: [String] -> Int
solvePartOne s = foldl (+) 0 $ map transformSafe $ map getRow s

transformSafe :: [Int] -> Int
transformSafe xs
 | isAdjacentDecreasing xs || isAdjacentIncreasing xs = 1
 | otherwise = 0

getRow :: String -> [Int]
getRow x = map read $ words x

isAdjacentDecreasing :: [Int] -> Bool
isAdjacentDecreasing (x:x':xs)
  | (x-x' >= 1) && (x-x' <= 3) = isAdjacentDecreasing (x':xs)
  | otherwise = False
isAdjacentDecreasing (_) = True

isAdjacentIncreasing :: [Int] -> Bool
isAdjacentIncreasing (x:x':xs)
  | (x'-x) >= 1 && (x'-x) <= 3 = isAdjacentIncreasing (x':xs)
  | otherwise = False
isAdjacentIncreasing (_) = True
