{-# LANGUAGE RecordWildCards #-}
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
    processorOperationTime <- liftSimulation $ newRef 0
    (processorProc, buffer) <- processor processorTotalTime processorOperationTime
    generatorProc <- generator buffer
    liftSimulation $ do
        runProcessInStartTime processorProc
        runProcessInStartTime generatorProc
        runEventInStopTime $ do
           sizeStats <- queueCountStats buffer
           lostCount <- fromIntegral <$> enqueueLostCount buffer
           totalCount <- fromIntegral <$> enqueueCount buffer
           awaiting <- queueWaitTime buffer
           totalTimeStats <- statsFromVar processorTotalTime
           operationTime <- readRef processorOperationTime
           
           let failChance' = lostCount / (lostCount + totalCount)
               queueSize' = timingStatsMean sizeStats
               systemLoad' = operationTime / simulationTime
               requestsCount' = queueSize' + systemLoad'
               awaitingTime' = samplingStatsMean awaiting
               totalTime' = samplingStatsMean totalTimeStats
           return $ Output failChance' queueSize' systemLoad' requestsCount' awaitingTime' totalTime' input
    where
    generator :: Buffer Request -> Event (Process ())
    generator buffer = do
        return $ forever $ do
           htime <- holdByDistribution generationDistribution
           when (htime >= 0) $ do
               timestamp <- liftDynamics time
               liftEvent $ do 
                   enqueueOrLost_ buffer $ Request timestamp

    processor :: Var Double -> Ref Double -> Event (Process (), Buffer Request)
    processor totalTimeVar operationTimeVar = withBuffer bufferCapacity $ \buffer-> forever $ do
       Request timestamp <- dequeue buffer
       operationTime <- holdPositive processingDistribution
       currentTime <- liftDynamics time
       liftEvent $ do
         writeVar totalTimeVar (currentTime - timestamp)
         modifyRef operationTimeVar (+operationTime)