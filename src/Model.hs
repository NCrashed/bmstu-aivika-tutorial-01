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

data Request = Request

simulate :: Input -> Simulation Output
simulate Input{..} = runEventInStartTime $ do
    rec (processorProc, buffer) <- processor stats
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
           return Output {
               failChance = lostCount / (lostCount + totalCount),
               queueSize = timingStatsMean sizeStats,
               requestsCount = samplingStatsMean requestsCountStats,
               awaitingTime = samplingStatsMean awaiting 
           }
    where
    generator :: Buffer Request -> Event (Process (), Var Int)
    generator buffer = do
        stats <- liftSimulation $ newVar 0
        return $ (, stats) $ forever $ do
           holdExponential generationTime
           liftEvent $ do
               modifyVar stats (+1)
               enqueueOrLost_ buffer Request

    processor :: Var Int -> Event (Process (), Buffer Request)
    processor stats = withBuffer bufferCapacity $ \buffer-> forever $ do
       _ <- dequeue buffer
       holdExponential processingTime
       liftEvent $ modifyVar stats (subtract 1)