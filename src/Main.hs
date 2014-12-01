{-# LANGUAGE RecordWildCards #-}
-- | Main entry point to the application.
module Main where

import Simulation.Aivika
import Simulation.Aivika.Queue
import Control.Monad
import Data.Functor
import Util

data Input = Input {
    generationTime :: Double,
    processingTime :: Double,
    bufferCapacity :: Int
}

testInput :: Input
testInput = Input {
    generationTime = 10,
    processingTime = 50,
    bufferCapacity = 2
}

data Output = Output {
    failChance :: Double,
    queueSize :: Double,
    requestsCount :: Double,
    awaitingTime :: Double
} deriving Show

data Request = Request

-- | The main entry point.
main :: IO ()
main = print =<< (flip runSimulation specs $ runEventInStartTime $ simulateProcess testInput)
    where specs = Specs {
              spcStartTime = 0.0,
              spcStopTime = 1000.0,
              spcDT = 10,
              spcMethod = RungeKutta4,
              spcGeneratorType = SimpleGenerator
          }   

simulateProcess :: Input -> Event Output
simulateProcess Input{..} = do
    (processorProc, buffer) <- processor 
    generatorProc <- generator buffer
    liftSimulation $ do
        runProcessInStartTime processorProc
        runProcessInStartTime generatorProc
        runEventInStopTime $ do
           sizeStats <- queueCountStats buffer
           lostCount <- fromIntegral <$> enqueueLostCount buffer
           totalCount <- fromIntegral <$> enqueueCount buffer
           dequed <- fromIntegral <$> dequeueCount buffer
           awaiting <- queueWaitTime buffer 
           return Output {
               failChance = lostCount / (lostCount + totalCount),
               queueSize = timingStatsMean sizeStats,
               requestsCount = dequed, --!!!!! HERE 
               awaitingTime = samplingStatsMean awaiting 
           }
    where
    generator :: Buffer Request -> Event (Process ())
    generator buffer = return $ forever $ do
           holdExponential generationTime
           liftEvent $ enqueueOrLost_ buffer Request

    processor :: Event (Process (), Buffer Request)
    processor = withBuffer bufferCapacity $ \buffer-> forever $ do
       dequeue buffer
       holdExponential processingTime