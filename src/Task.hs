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
    failChance :: Double, -- ^ Вероятность отказа системы
    queueSize :: Double, -- ^ Средний размер буфера
    systemLoad :: Double, -- ^ Загрузка системы
    requestsCount :: Double, -- ^ Среднее число заявок в системе
    awaitingTime :: Double, -- ^ Среднее время ожидания в буфере
    totalTime :: Double, -- ^ Общее время пребывания заявки в системе
    usedInput :: Input -- ^ Используемые входные данные
}

instance Show Output where
    show Output{..} = 
        printf ("Вероятность отказа: %."++precision++"f\n") failChance ++ 
        printf ("Средний размер буфера: %."++precision++"f\n") queueSize ++ 
        printf ("Загрузка системы: %."++precision++"f\n") systemLoad ++ 
        printf ("Среднее число заявок в системе: %."++precision++"f\n") requestsCount ++ 
        printf ("Среднее время ожидания в буфере: %."++precision++"f\n") awaitingTime ++
        printf ("Общее время пребывания заявки в системе: %."++precision++"f\n") totalTime
        where
        precision = show (outputPrecision usedInput)