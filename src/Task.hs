{-# LANGUAGE RecordWildCards #-}
module Task where

import Text.Printf
import Simulation.Aivika
import Data.Functor

data System = System { 
    processingDistribution :: (String, Parameter Double)
  , bufferCapacity :: Int
}

data Input = Input {
    generationDistribution :: (String, Parameter Double)
  , inputSystems :: [System]
  , simulationTime :: Double
  , outputPrecision :: Int
}

instance Show System where
  show System{..} = fst processingDistribution ++ "-" ++ show bufferCapacity
  
instance Show Input where
  show Input{..} = fst generationDistribution ++ "-" ++ show inputSystems
    
data Output = Output {
    failChances :: [Double], -- ^ Вероятность отказа системы
    queueSizes :: [Double], -- ^ Средний размер буфера
    systemLoads :: [Double], -- ^ Загрузка системы
    requestsCounts :: [Double], -- ^ Среднее число заявок в системе
    awaitingTimes :: [Double], -- ^ Среднее время ожидания в буфере
    totalTimes :: [Double], -- ^ Общее время пребывания заявки в системе
    usedInput :: Input -- ^ Используемые входные данные
}

emptyOutput :: Input ->  Output
emptyOutput input = Output [] [] [] [] [] [] input  
  
data PartialOutput = PartialOutput {
    failChance :: Double, -- ^ Вероятность отказа системы
    queueSize :: Double, -- ^ Средний размер буфера
    systemLoad :: Double, -- ^ Загрузка системы
    requestsCount :: Double, -- ^ Среднее число заявок в системе
    awaitingTime :: Double, -- ^ Среднее время ожидания в буфере
    totalTime :: Double -- ^ Общее время пребывания заявки в системе
}

combineOutput :: Output -> PartialOutput -> Output
combineOutput output poutput = output {
    failChances = failChances output ++ [failChance poutput]
  , queueSizes = queueSizes output ++ [queueSize poutput]
  , systemLoads = systemLoads output ++ [systemLoad poutput]
  , requestsCounts = requestsCounts output ++ [requestsCount poutput]
  , awaitingTimes = awaitingTimes output ++ [awaitingTime poutput]
  , totalTimes = totalTimes output ++ [totalTime poutput]
  }

combineOutputs :: Output -> [PartialOutput] -> Output
combineOutputs output = foldl combineOutput output

instance Show Output where
    show Output{..} = unlines [
          unwords ["Вероятность отказа в каждом буфере:", unwords $ printPrec <$> failChances] 
        , unwords ["Средний размер буферов:", unwords $ printPrec <$> queueSizes] 
        , unwords ["Загрузка подсистем:", unwords $ printPrec <$> systemLoads]
        , unwords ["Среднее число заявок в системах:", unwords $ printPrec <$> requestsCounts]
        , unwords ["Среднее время ожидания в буфере:", unwords $ printPrec <$> awaitingTimes]
        , unwords ["Общее время пребывания заявки в системе:", unwords $ printPrec <$> totalTimes]
        ]
        where
        precision = show (outputPrecision usedInput)

        printPrec :: Double -> String
        printPrec = printf ("%."++precision++"f")