module Main where

import Day1 (day01)
import Day2 (day02)
import Day3 (day03)
import Day4 (day04)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    "01": _ -> day01
    "02": _ -> day02
    "03": _ -> day03
    "04": _ -> day04
    _ -> error "computer self destruction alert: 10 seconds"
