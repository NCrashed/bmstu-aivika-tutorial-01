{-# LANGUAGE RecordWildCards #-}
module Task where

import Text.Printf
import Simulation.Aivika
import Data.Functor

data Input = Input {
    generationDistribution :: Parameter Double,
    processingDistribution :: Parameter Double,
    bufferCapacity :: [Int],
    simulationTime :: Double,
    outputPrecision :: Int
}

data Output = Output {
    failChances :: [Double], -- ^ Вероятность отказа системы
    queueSizes :: [Double], -- ^ Средний размер буфера
--    systemLoad :: Double, -- ^ Загрузка системы
--    requestsCount :: Double, -- ^ Среднее число заявок в системе
--    awaitingTime :: Double, -- ^ Среднее время ожидания в буфере
--    totalTime :: Double, -- ^ Общее время пребывания заявки в системе
    usedInput :: Input -- ^ Используемые входные данные
}

emptyOutput :: Input ->  Output
emptyOutput input = Output [] [] input  
  
data PartialOutput = PartialOutput {
    failChance :: Double, -- ^ Вероятность отказа системы
    queueSize :: Double -- ^ Средний размер буфера
--    systemLoad :: Double, -- ^ Загрузка системы
--    requestsCount :: Double, -- ^ Среднее число заявок в системе
--    awaitingTime :: Double, -- ^ Среднее время ожидания в буфере
--    totalTime :: Double, -- ^ Общее время пребывания заявки в системе
}

combineOutput :: Output -> PartialOutput -> Output
combineOutput output poutput = output {
    failChances = failChances output ++ [failChance poutput]
  , queueSizes = queueSizes output ++ [queueSize poutput]
  }

combineOutputs :: Output -> [PartialOutput] -> Output
combineOutputs output = foldl combineOutput output

instance Show Output where
    show Output{..} = unlines [
          unwords ["Вероятность отказа в каждом буфере:", unwords $ printPrec <$> failChances] 
        , unwords ["Средний размер буфера:", unwords $ printPrec <$> queueSizes] 
--        printf ("Загрузка системы: %."++precision++"f\n") systemLoad ++ 
--        printf ("Среднее число заявок в системе: %."++precision++"f\n") requestsCount ++ 
--        printf ("Среднее время ожидания в буфере: %."++precision++"f\n") awaitingTime ++
--        printf ("Общее время пребывания заявки в системе: %."++precision++"f\n") totalTime
        ]
        where
        precision = show (outputPrecision usedInput)

        printPrec :: Double -> String
        printPrec = printf ("%."++precision++"f")