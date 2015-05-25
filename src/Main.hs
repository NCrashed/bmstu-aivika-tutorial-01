module Main where

import Model
import Tabular
import Varying
import Parser

main :: IO ()
main = do
  minputs <- parseFile "input.txt"
  case minputs of
    Left err -> print err
    Right inputs ->
      putStrLn.prettyPrintOutputs =<< simulateMany inputs simulate