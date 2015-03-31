{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
module Model where

import Simulation.Aivika
import Simulation.Aivika.Queue
import Control.Monad
import Data.Functor
import Util
import Task

-- | Тип, описывающий заявку, внутри хранит время, когда заявка была создана
data Request = Request Double

simulate :: Input -> Simulation Output
simulate input@Input{..} = runEventInStartTime $ do
    processorTotalTime <- liftSimulation $ newVar 0
    rec (processorProc, buffer) <- processor stats processorTotalTime
        (generatorProc, stats) <- generator buffer
    liftSimulation $ do
        runProcessInStartTime processorProc
        runProcessInStartTime generatorProc
        runEventInStopTime $ do
           sizeStats <- queueCountStats buffer
           lostCount <- fromIntegral <$> enqueueLostCount buffer
           totalCount <- fromIntegral <$> enqueueCount buffer
           awaiting <- queueWaitTime buffer
           requestsCountStats <- statsFromVar stats
           totalTimeStats <- statsFromVar processorTotalTime
           return Output {
               failChance = lostCount / (lostCount + totalCount),
               queueSize = timingStatsMean sizeStats,
               requestsCount = samplingStatsMean requestsCountStats,
               awaitingTime = samplingStatsMean awaiting,
               totalTime = samplingStatsMean totalTimeStats,
               usedInput = input
           }
    where
    generator :: Buffer Request -> Event (Process (), Var Int)
    generator buffer = do
        stats <- liftSimulation $ newVar 0
        return $ (, stats) $ forever $ do
           holdByDistribution generationDistribution
           timestamp <- liftDynamics time
           liftEvent $ do
               modifyVar stats (+1) 
               enqueueOrLost_ buffer $ Request timestamp

    processor :: Var Int -> Var Double -> Event (Process (), Buffer Request)
    processor stats totalTimeVar = withBuffer bufferCapacity $ \buffer-> forever $ do
       Request timestamp <- dequeue buffer
       holdByDistribution processingDistribution
       currentTime <- liftDynamics time
       liftEvent $ do
         writeVar totalTimeVar (currentTime - timestamp)
         modifyVar stats (subtract 1)
      