{-# LANGUAGE RecordWildCards #-}
module Task where

import Text.Printf
import Simulation.Aivika

data Input = Input {
    generationDistribution :: Parameter Double,
    processingDistribution :: Parameter Double,
    bufferCapacity :: Int,
    simulationTime :: Double,
    outputPrecision :: Int
}

data Output = Output {
    failChance :: Double,
    queueSize :: Double,
    requestsCount :: Double,
    awaitingTime :: Double,
    totalTime :: Double, -- ^ Общее время пребывания заявки в системе
    usedInput :: Input
}

instance Show Output where
    show Output{..} = 
        printf ("Вероятность отказа: %."++precision++"f\n") failChance ++ 
        printf ("Средний размер буфера: %."++precision++"f\n") queueSize ++ 
        printf ("Среднее число заявок в системе: %."++precision++"f\n") requestsCount ++ 
        printf ("Среднее время ожидания в буфере: %."++precision++"f\n") awaitingTime ++
        printf ("Общее время пребывания заявки в системе: %."++precision++"f\n") totalTime
        where
        precision = show (outputPrecision usedInput)