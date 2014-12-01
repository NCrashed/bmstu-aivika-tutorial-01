{-# LANGUAGE RecordWildCards #-}
-- | Main entry point to the application.
module Main where

import Simulation.Aivika
import Simulation.Aivika.Queue
import Control.Monad
import Util

data Input = Input {
    generationTime :: Double,
    processingTime :: Double,
    bufferCapacity :: Int
}

testInput :: Input
testInput = Input {
    generationTime = 80,
    processingTime = 50,
    bufferCapacity = 10
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
              spcDT = 1.0,
              spcMethod = RungeKutta4,
              spcGeneratorType = SimpleGenerator
          }   

simulateProcess :: Input -> Event Output
simulateProcess Input{..} = do
    (processorProc, buffer) <- processor
    liftSimulation $ do
        runProcessInStartTime processorProc
        runProcessInStartTime $ generator buffer
        runEventInStopTime $ do
           
            return Output {
                failChance = 0.0,
                queueSize = 0.0,
                requestsCount = 0.0,
                awaitingTime = 0.0
            }
    where
    generator :: Buffer Request -> Process ()
    generator buffer = forever $ do
       holdExponential generationTime
       enqueue buffer Request

    processor :: Event (Process (), Buffer Request)
    processor = withBuffer bufferCapacity $ \buffer-> forever $ do
       dequeue buffer