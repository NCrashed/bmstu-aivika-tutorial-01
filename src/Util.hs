module Util where

import Simulation.Aivika
import Simulation.Aivika.Queue
import Simulation.Aivika.Unboxed
import Data.Array
import Control.Monad.Random
import Control.Monad.IO.Class
import Control.Arrow

type Buffer a = FCFSQueue a

holdByDistribution :: (Parameter Double) -> Process ()
holdByDistribution f = do
    htime <- liftParameter f
    holdProcess htime

newBuffer :: Int -> Event (Buffer a)
newBuffer = newFCFSQueue 

withBuffer :: Int -> (Buffer a -> Process b) -> Event (Process b, Buffer a)
withBuffer capacity f = do
   buffer <- newBuffer capacity
   return (f buffer, buffer)

statsFromVar :: (Unboxed a, SamplingData a) => Var a -> Event (SamplingStats a)
statsFromVar var = do
    (_, vals, _) <- freezeVar var
    return $ listSamplingStats (elems vals)

randomHyperExponential :: [(Double, Double)] -> Parameter Double
randomHyperExponential pairs = liftIO $ do 
    rng <- newStdGen
    mean <- evalRandT (fromList $ map (second toRational) pairs) rng
    return mean