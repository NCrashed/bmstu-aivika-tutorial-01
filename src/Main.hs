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

main :: IO ()
main = print =<< runSimulation (simulate testInput) specs
    where specs = Specs {
              spcStartTime = 0.0,
              spcStopTime = simulationTime testInput,
              spcDT = 10,
              spcMethod = RungeKutta4,
              spcGeneratorType = SimpleGenerator
          }   