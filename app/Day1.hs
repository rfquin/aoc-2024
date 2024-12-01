module Day1 where

import Data.List

day01 :: IO()
day01 = do
  input <- lines <$> readFile "inputs/day1-input.txt"
  putStrLn $ show (solvePartTwo input)

-- part two solution

solvePartTwo :: [String] -> Int
solvePartTwo s = foldl (+) 0 $ similarity (getFirst s, getSecond s)

similarity :: ([Int], [Int]) -> [Int]
similarity (xs,ys) = map (\x -> x * (getAppearences x ys)) xs

getAppearences :: Int -> [Int] -> Int
getAppearences x ys = foldl (\acc x' -> if (x == x') then acc + 1 else acc) 0 ys

-- part one solution

solvePartOne :: [String] -> Int
solvePartOne xs = foldl (+) 0 (differences xs)

differences :: [String] -> [Int]
differences s = compress (sort $ getFirst s, sort $ getSecond s)

compress :: ([Int], [Int]) -> [Int]
compress (xs, ys) = map (\(x,y) -> abs $ x - y) (zip xs ys)

getFirst :: [String] -> [Int]
getFirst = map (\x -> read $ head $ words x)

getSecond :: [String] -> [Int]
getSecond = map (\x -> read $ last $ words x)
