module Day3 where

import Data.Char
import Control.Applicative

newtype Parser a = Parser (String -> Maybe (String, a))

parse :: Parser a -> String -> Maybe (String,a)
parse (Parser a) s = a s

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (fmap f) . p

instance Applicative Parser where
  pure x = Parser $ \s -> Just (s,x)
  Parser f <*> Parser x =
    Parser $ \s ->
    do
      (s',f') <- f s
      fmap f' <$> x s'

instance Monad Parser where
  return = pure
  Parser p >>= f =
    Parser $ \s ->
    do
      (s',a) <- p s
      let Parser p' = f a
      (p' s')

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser p1 <|> Parser p2 =
    Parser $ \s -> p1 s <|> p2 s

clean :: Parser String
clean = Parser $ \s -> case s of
  "" -> Just ("", "")
  _ -> if parse (parseMul 1 <|> parseCondition) s == Nothing
  then parse clean (drop 1 s)
  else Just (s, "")

parseLine :: Int -> Parser [Int]
parseLine state = do
  _ <- clean
  state' <- parseCondition <|> return state  -- If do / don't found, change to new state. Otherwise, use previous.
  result <- parseMul state' <|> return 0 -- If mul found, use it. Else, add zero to count.
  rest <- failNotEmpty <|> parseLine state'
  return (result:rest)

failNotEmpty :: Parser [Int]
failNotEmpty = Parser $ \s -> if (s == "") then Just ("", []) else Nothing

parseCondition :: Parser Int
parseCondition = Parser $ \s -> case s of
  "" -> Just ("", 1)
  _ -> case parse (parseString "do()" <|> parseString "don't()") s of
    Just (rest, "do()") -> Just (rest, 1)
    Just (rest, "don't()") -> Just (rest, 0)
    _ -> Nothing

parseMul :: Int -> Parser Int
parseMul isValid = do
  _ <- parseString "mul"
  _ <- parseChar '('
  p1 <- parseInt
  _ <- parseChar ','
  p2 <- parseInt
  _ <- parseChar ')'
  return (p1 * p2 * isValid) -- If don't, times expression by zero to add 0 to total

parseString :: String -> Parser String
parseString = sequenceA . map parseChar

parseChar :: Char -> Parser Char
parseChar p = Parser $ \s -> case s of
  (x:xs) -> if (x == p) then Just (xs,p) else Nothing
  _ -> Nothing

parseInt :: Parser Int
parseInt = Parser $ \s ->
  case s of
    (x:_) | isDigit x ->
      case parse parseDigits s of
        Just (rest, digits) -> Just (rest, read digits)
        Nothing -> Nothing
    _ -> Nothing

parseDigits :: Parser String
parseDigits = Parser $ \s -> let (dig, rest) = span isDigit s in Just (rest, dig)

day03 :: IO()
day03 = do
  input <- readFile "inputs/day3-input.txt"
  case parse (parseLine 1) input of
    Just (_, results) -> putStrLn $ show $ sum (results)
    Nothing -> putStrLn "oopsie"
