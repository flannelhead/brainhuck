module Main where

import System.Environment

import Brainfuck

main :: IO ()
main = do
    argv <- getArgs
    case argv of
      [s] -> readFile s >>= parseAndRun
      _   -> putStrLn "Usage: brainhuck <filename>"
