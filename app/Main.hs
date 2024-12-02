module Main where

import Day1 (day01)
import Day2 (day02)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "01": _ -> day01
    "02": _ -> day02
    _ -> error "Sigma sigma on the wall"
