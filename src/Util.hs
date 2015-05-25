{-# LANGUAGE TupleSections #-}
module Util where

import Simulation.Aivika
import Simulation.Aivika.Queue
import Simulation.Aivika.Unboxed
import Data.Array
import Control.Monad
import Control.Monad.Random
import Control.Monad.IO.Class
import Control.Arrow

type Buffer a = FCFSQueue a

holdByDistribution :: (Parameter Double) -> Process Double
holdByDistribution f = do
    htime <- liftParameter f
    when (htime > 0) $ holdProcess htime
    return htime

holdPositive :: (Parameter Double) -> Process Double
holdPositive f = do
    htime <- holdByDistribution f
    if htime >= 0
    then return htime
    else holdPositive f
    
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

generationDistr :: String -> [Double] -> (String, Parameter Double)
generationDistr "exponential" [meanTime] = ("exponential("++show meanTime++")", randomExponential meanTime)
generationDistr "exponential" _ = error "Нужен 1 параметр экспоненциальному распределению"
generationDistr "erlang" [theta, k] = ("erlang("++show theta++","++show k++")",) $ do
    let ki = floor k
    val <- randomErlang theta ki
    return $ val / fromIntegral ki
generationDistr "erlang" _ = error "Нужно 2 параметра эрланговскому распределению: theta (плавающая запятая) и k (целочисленное)"
generationDistr "normal" [mu, sigma] = ("normal("++show mu++","++show sigma++")", randomNormal mu sigma)
generationDistr "normal" _ = error "Нужно 2 параметра нормальному распределению: mu (математическое ожидание) и sigma (дисперсия)"
generationDistr "uniform" [minVal, maxVal] = ("uniform"++show minVal++","++show maxVal++")", randomUniform minVal maxVal)
generationDistr "uniform" _ = error "Нужно 2 параметра равномерному распределению: minVal (минимальное значение) и maxVal (максимальное значение)"
generationDistr "hyperexponential" params 
    | length params `mod` 2 /= 0 = error "Необходимо четное количество параметров гиперэскпоненциального распределения: пара состоит из среднего времени и вероятности."
    | otherwise = if abs (sumSeconds - 1.0) < 0.001
        then ("hyperexponential("++show pairs++")", randomHyperExponential pairs)
        else error "Сумма вероятностей вариантов в гиперэкспоненциальном распределении не равна 1.0"
        where 
            pairs = makePairs params
            sumSeconds = sum $ map snd pairs
            makePairs (a:b:xs) = (a,b): makePairs xs
            makePairs _ = []
generationDistr _ _ = error "Неизвестный закон распределения"