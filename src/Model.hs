{-# LANGUAGE RecordWildCards #-}
module Model where

import Simulation.Aivika
import Simulation.Aivika.Queue
import Control.Monad
import Control.Applicative
import Util
import Task

-- | Тип, описывающий заявку, внутри хранит время, когда заявка была создана
data Request = Request Double

simulate :: Input -> Simulation Output
simulate input = runEventInStartTime $ do
  outputs <- cascadeSMO input
  liftSimulation $ runEventInStopTime $ do
    combineOutputs (emptyOutput input) <$> outputs
    
cascadeSMO :: Input -> Event (Event [PartialOutput])
cascadeSMO input = do
  (terProc, termBuff) <- terminator
  liftSimulation $ runProcessInStartTime terProc
  (firstBuff, outputs) <- foldM go (termBuff, []) $ reverse $ bufferCapacity input
  genProc <- generator firstBuff
  liftSimulation $ runProcessInStartTime genProc
  return $ sequence outputs
  where
  terminator :: Event (Process (), Buffer Request)
  terminator = withBuffer 1000000 $ \buffer-> forever $ do
    _ <- dequeue buffer
    return ()

  generator :: Buffer Request -> Event (Process ())
  generator buffer = do
    return $ forever $ do
      htime <- holdByDistribution $ generationDistribution input
      when (htime >= 0) $ do
        timestamp <- liftDynamics time
        liftEvent $ do 
          enqueueOrLost_ buffer $ Request timestamp
          
  go :: 
    (Buffer Request, [Event PartialOutput]) -> Int 
    -> Event (Buffer Request, [Event PartialOutput])   
  go (nextBuffer, failChanceAcc) cap = do
    (b, fc) <- atomSMO input cap nextBuffer
    return $ (b, fc:failChanceAcc)
  
atomSMO :: Input -> Int -> Buffer Request -> Event (Buffer Request, Event PartialOutput)
atomSMO input buffCap nextBuffer = do 
  processorOperationTime <- liftSimulation $ newRef 0
  (processorProc, buffer) <- processor nextBuffer processorOperationTime
  liftSimulation $ runProcessInStartTime processorProc
  let failChance = do
        lostCount <- fromIntegral <$> enqueueLostCount buffer
        totalCount <- fromIntegral <$> enqueueCount buffer
        return $! lostCount / (lostCount + totalCount)
  let qSize = do
        sizeStats <- queueCountStats buffer
        return $! timingStatsMean sizeStats
  let sLoad = do
        operationTime <- readRef processorOperationTime
        return $! operationTime / simulationTime input
  return (buffer, PartialOutput <$> failChance <*> qSize <*> sLoad)
  where
    processor :: Buffer Request -> Ref Double -> Event (Process (), Buffer Request)
    processor outBuffer operationTimeVar = withBuffer buffCap $ \buffer-> forever $ do
      r <- dequeue buffer
      operationTime <- holdPositive $ processingDistribution input
      liftEvent $ do
        modifyRef operationTimeVar (+operationTime)
        enqueueOrLost_ outBuffer r
         
{-
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
         modifyRef operationTimeVar (+operationTime) -}