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
  processorTotalTime <- liftSimulation $ newVar 0
  processorOperationTime <- liftSimulation $ newRef 0
  (processorProc, buffer) <- processor nextBuffer processorTotalTime processorOperationTime
  liftSimulation $ runProcessInStartTime processorProc
  return (buffer, PartialOutput 
    <$> failChance buffer
    <*> qSize buffer
    <*> sLoad processorOperationTime
    <*> reqCount buffer processorOperationTime
    <*> awaitTime buffer
    <*> totalTime processorTotalTime
    )
  where
    processor :: Buffer Request -> Var Double -> Ref Double -> Event (Process (), Buffer Request)
    processor outBuffer totalTimeVar operationTimeVar = withBuffer buffCap $ \buffer-> forever $ do
      r@(Request timestamp) <- dequeue buffer
      operationTime <- holdPositive $ processingDistribution input
      currentTime <- liftDynamics time
      liftEvent $ do
        writeVar totalTimeVar (currentTime - timestamp)
        modifyRef operationTimeVar (+operationTime)
        enqueueOrLost_ outBuffer r
        
    failChance buffer = do
      lostCount <- fromIntegral <$> enqueueLostCount buffer
      totalCount <- fromIntegral <$> enqueueCount buffer
      return $! lostCount / (lostCount + totalCount)
      
    qSize buffer = do
      sizeStats <- queueCountStats buffer
      return $! timingStatsMean sizeStats
      
    sLoad processorOperationTime = do
      operationTime <- readRef processorOperationTime
      return $! operationTime / simulationTime input
      
    reqCount buffer processorOperationTime = do
      queueSize' <- qSize buffer
      systemLoad' <- sLoad processorOperationTime
      return $! queueSize' + systemLoad'
      
    awaitTime buffer = do
      awaiting <- queueWaitTime buffer
      return $! samplingStatsMean awaiting
      
    totalTime processorTotalTime = do
      totalTimeStats <- statsFromVar processorTotalTime
      return $! samplingStatsMean totalTimeStats