{-# LANGUAGE RecordWildCards #-}
module Parser where

import Text.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Task
import Util
import Control.Applicative ((<$>), (<*>))
import Control.Monad (void)

type Parser a = Parsec String () a

parseFile :: FilePath -> IO (Either ParseError [Input])
parseFile fname = do 
  input <- readFile fname
  return $ runParser (inputParser haskell) () fname input
  
inputParser :: TokenParser () -> Parser [Input]
inputParser TokenParser{..} = do
  whiteSpace
  many $ lexeme parseExperiment
  where
    parseExperiment = do 
      void $ symbol "experiment" 
      between (symbol "{") (symbol "}") $ Input
        <$> (symbol "input" >> parseDisrt)
        <*> many (try parseSystem)
        <*> parseSimulationTime
        <*> (fromInteger <$> parsePrecision)

    parseSimulationTime = symbol "simulation" >> symbol "time" >> float'
    parsePrecision = symbol "output" >> symbol "precision" >> natural

    parseSystem :: Parser System
    parseSystem = do
      void $ symbol "subsystem"
      between (symbol "{") (symbol "}") $ System
        <$> (symbol "processing" >> parseDisrt)
        <*> (symbol "buffer" >> (fromInteger <$> natural))
      
    parseDisrt = choice [
        try erlangParser
      , expParser
      , normalParser
      , uniformParser
      , hyperExpParser]
    
    erlangParser = do
      void $ symbol "erlang"
      t <- float'
      k <- natural
      return $ generationDistr "erlang" [t, fromIntegral k]

    expParser = do
      void $ symbol "exponent"
      t <- float'
      return $ generationDistr "exponential" [t]

    normalParser = do
      void $ symbol "normal"
      mu <- float'
      sigma <- float'
      return $ generationDistr "normal" [mu, sigma]

    uniformParser = do
      void $ symbol "uniform"
      minv <- float'
      maxv <- float'
      return $ generationDistr "uniform" [minv, maxv]

    hyperExpParser = do
      void $ symbol "hyperexponent"
      pairs <- many $ between (symbol "(") (symbol ")") $ do
        t <- float'
        void $ symbol ","
        p <- float'
        return (t, p)
      return $ generationDistr "hyperexponential" $ concat $ (\(a,b)->[a,b]) <$> pairs

    float' = do
      mv <- naturalOrFloat
      return $ case mv of
        Left i -> fromIntegral i
        Right v -> v
      