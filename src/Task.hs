{-# LANGUAGE RecordWildCards #-}
module Task where

import Text.Printf

data Input = Input {
    generationTime :: Double,
    processingTime :: Double,
    bufferCapacity :: Int
}

data Output = Output {
    failChance :: Double,
    queueSize :: Double,
    requestsCount :: Double,
    awaitingTime :: Double
}

instance Show Output where
    show Output{..} = 
        printf "Вероятность отказа: %.2f\n" failChance ++ 
        printf "Средний размер буфера: %.2f\n" queueSize ++ 
        printf "Среднее число заявок в системе: %.2f\n" requestsCount ++ 
        printf "Среднее время ожидания в буфере: %.2f\n" awaitingTime