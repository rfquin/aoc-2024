module Day4 where

day04 :: IO()
day04 = do
  s <- lines <$> readFile "inputs/day4-input.txt"
  print $ count s

newtype Direction a = Direction { run :: (Int, a) -> (Int, a)}

count :: [[Char]] -> Int
count w = diagonals w + columns w + rows w

diagonals :: [[Char]] -> Int
diagonals w = diagonal_reverse w + diagonal_straight w

diagonal_reverse :: [[Char]] -> Int
diagonal_reverse w@(s:_) = sum (map (getHorizontal . reverse) (getLeftDiagonals 0 w)) +
  sum (map (getHorizontal . reverse) (getRightDiagonals (length s - 1) w))
diagonal_reverse _ = 0

diagonal_straight :: [[Char]] -> Int
diagonal_straight w@(s:_) = sum (map (getHorizontal) (getLeftDiagonals 0 w)) +
  sum (map (getHorizontal) (getRightDiagonals (length s - 1) w))
diagonal_straight _ = 0

getRightDiagonals :: Int -> [[Char]] -> [[Char]]
getRightDiagonals _ [] = []
getRightDiagonals x w = case getRightDiagonal x w of
  [] -> []
  _ -> getRightDiagonal x w : getRightDiagonals (x-1) w

getLeftDiagonals :: Int -> [[Char]] -> [[Char]]
getLeftDiagonals _ [] = []
getLeftDiagonals x w = case getLeftDiagonal x w of
  [] -> []
  _ -> getLeftDiagonal x w : getLeftDiagonals (x+1) w

getRightDiagonal :: Int -> [[Char]] -> [Char]
getRightDiagonal _ [] = ""
getRightDiagonal x (s:ss) = case fromIndex x s of
  Just c -> c : getRightDiagonal (x-1) ss
  Nothing -> []

getLeftDiagonal :: Int -> [[Char]] -> [Char]
getLeftDiagonal _ [] = ""
getLeftDiagonal x (s:ss) = case fromIndex x s of
  Just c -> c : getLeftDiagonal (x+1) ss
  Nothing -> []

fromIndex :: Int -> [Char] -> Maybe Char
fromIndex x s = if (x < 0) then Nothing else
  case length s > x of
  True -> Just $ head $ drop x s
  _ -> Nothing

rows :: [[Char]] -> Int
rows (s:ss) = (a + b + rows (ss)) where
  ((a,_), (b,_)) = (run horizontal (0,s), run horizontal (0, reverse s))
rows _ = 0 -- Finished parsing

columns :: [[Char]] -> Int
columns w = vertical w + vertical_reverse w

getHorizontal :: [Char] -> Int
getHorizontal s = x where (x,_) = run horizontal (0, s)

horizontal :: Direction [Char]
horizontal = Direction $
  \(acc,s) -> case s of
    "" -> (acc, s) -- Finished parsing
    _ -> if (take 4 s == "XMAS")
      then run horizontal (acc+1, drop 4 s) -- Add one to count, remove word from string
      else run horizontal (acc, drop 1 s) -- Try again after removing one character

vertical :: [[Char]] -> Int
vertical w = sum $ map (getHorizontal) (getVerticals w)

vertical_reverse :: [[Char]] -> Int
vertical_reverse w = sum $ map (getHorizontal . reverse) (getVerticals w)

getVerticals :: [[Char]] -> [[Char]]
getVerticals w = w' where (_,w') = arrangeVertical (0,w)

arrangeVertical :: (Int, [[Char]]) -> (Int, [[Char]])
arrangeVertical (i, w@(s:_)) = case (drop i s) of
  [] -> (i, []) -- Stop the column finding recursive chain if we've dropped everything
  _ -> (0, (getColumn i w) : rest) where (_, rest) = (arrangeVertical (i+1,w))
arrangeVertical (i, _) = (i, [[]])

getColumn :: Int -> [[Char]] -> [Char]
getColumn i (s:ss) = (head $ drop i s) : (getColumn i ss)
getColumn _ [] = "" -- End of recursion
