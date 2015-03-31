module Main where

import Simulation.Aivika
import Task
import Model
import Util

-- | Входные параметры модели
testInput :: Input
testInput = Input {
    -- | Закон распределения потока заявок
    -- Указывается один из: exponential, erlang, normal, uniform
    -- каждому закону нужно указать список параметров
    -- для exponential нужен 1 параметр (среднее время между заявками)
    -- для других (erlang и т.д.) нужно по 2 параметра
    generationDistribution = generationDistr "hyperexponential" [10, 0.5, 120, 0.5],
    -- | Закон распределения обработки заявок в обслуживающем автомате
    -- параметры аналогичны.
    processingDistribution = generationDistr "exponential" [50],
    -- | Емкость буффера
    bufferCapacity = 2,
    -- | Время иммитационного моделирования
    simulationTime = 500000.0,
    -- | Количество знаков после запятой в выводе результатов
    outputPrecision = 5
}

-- ===================================================================

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

main :: IO ()
main = print =<< runSimulation (simulate testInput) specs
    where specs = Specs {
              spcStartTime = 0.0,
              spcStopTime = simulationTime testInput,
              spcDT = 10,
              spcMethod = RungeKutta4,
              spcGeneratorType = SimpleGenerator
          }   