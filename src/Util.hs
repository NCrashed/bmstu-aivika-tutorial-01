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

generationDistr :: String -> [Double] -> Parameter Double
generationDistr "exponential" [meanTime] = randomExponential meanTime
generationDistr "exponential" _ = error "Нужен 1 параметр экспоненциальному распределению"
generationDistr "erlang" [theta, k] = randomErlang theta (floor k)
generationDistr "erlang" _ = error "Нужно 2 параметра эрланговскому распределению: theta (плавающая запятая) и k (целочисленное)"
generationDistr "normal" [mu, sigma] = randomNormal mu sigma
generationDistr "normal" _ = error "Нужно 2 параметра нормальному распределению: mu (математическое ожидание) и sigma (дисперсия)"
generationDistr "uniform" [minVal, maxVal] = randomUniform minVal maxVal
generationDistr "uniform" _ = error "Нужно 2 параметра равномерному распределению: minVal (минимальное значение) и maxVal (максимальное значение)"
generationDistr "hyperexponential" params 
    | length params `mod` 2 /= 0 = error "Необходимо четное количество параметров гиперэскпоненциального распределения: пара состоит из среднего времени и вероятности."
    | otherwise = if abs (sumSeconds - 1.0) < 0.001
        then randomHyperExponential pairs
        else error "Сумма вероятностей вариантов в гиперэкспоненциальном распределении не равна 1.0"
        where 
            pairs = makePairs params
            sumSeconds = sum $ map snd pairs
            makePairs (a:b:xs) = (a,b): makePairs xs
            makePairs _ = []
generationDistr _ _ = error "Неизвестный закон распределения"