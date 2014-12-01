module Util where

import Simulation.Aivika
import Simulation.Aivika.Queue

type Buffer a = FCFSQueue a

holdExponential :: Double -> Process ()
holdExponential t = do
    htime <- liftParameter $ randomExponential t
    holdProcess htime

newBuffer :: Int -> Event (Buffer a)
newBuffer = newFCFSQueue 

withBuffer :: Int -> (Buffer a -> Process b) -> Event (Process b, Buffer a)
withBuffer capacity f = do
   buffer <- newBuffer capacity
   return (f buffer, buffer)

