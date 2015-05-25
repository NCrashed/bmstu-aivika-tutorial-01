module Main where

import Model
import Tabular
import Varying
import Parser
import System.Environment
import System.IO 

parseArgs :: IO (FilePath, FilePath)
parseArgs = do 
  margs <- getArgs
  case margs of
    [] -> return ("input.txt", "output.txt")
    [input] -> return (input, "output.txt")
    (input:output:_) -> return (input, output)

main :: IO ()
main = do
  (inputPath, outputPath) <- parseArgs
  minputs <- parseFile inputPath
  case minputs of
    Left err -> print err
    Right inputs -> do
      outputs <- simulateMany inputs simulate
      let str = prettyPrintOutputs outputs
      putStrLn str
      withFile outputPath WriteMode $ \h -> do
        hSetEncoding h utf8_bom
        hPutStr h str
      putStrLn $ "Results were written to " ++ outputPath