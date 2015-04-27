{-# LANGUAGE RecordWildCards #-}
module Tabular where

import Control.Applicative
import Text.Tabular
import Text.Tabular.AsciiArt
import Text.Printf
import Task

toTabular :: Output -> Table String String Double
toTabular Output{..} = Table 
  ( Group NoLine $ Header <$> [
      "Вероятность отказа в каждом буфере:" 
    , "Средний размер буферов:"
    , "Загрузка подсистем:"
    , "Среднее число заявок в системах:"
    , "Среднее время ожидания в буфере:"
    , "Общее время пребывания заявки в системе:"
    ]
  )
  ( Group SingleLine $ Header . show <$> [1 .. length (bufferCapacity usedInput)]
  )
  [
    failChances
  , queueSizes
  , systemLoads
  , requestsCounts
  , awaitingTimes
  , totalTimes
  ]


prettyPrintOutput :: Output -> String
prettyPrintOutput output = render id id printPrec $
  toTabular output
  where
    precision = show (outputPrecision $ usedInput output)

    printPrec :: Double -> String
    printPrec = printf ("%."++precision++"f")