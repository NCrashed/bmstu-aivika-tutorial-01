module Main where

import Task
import Model
import Util
import Tabular
import Varying

-- | Входные параметры модели
baseInput :: Input
baseInput = Input {
    -- | Закон распределения потока заявок
    -- Указывается один из: exponential, erlang, normal, uniform
    -- каждому закону нужно указать список параметров
    -- для exponential нужен 1 параметр (среднее время между заявками)
    -- для других (erlang и т.д.) нужно по 2 параметра
    generationDistribution = generationDistr "erlang" [10, 2],
    inputSystems = [],
    -- | Время иммитационного моделирования
    simulationTime = 500000.0,
    -- | Количество знаков после запятой в выводе результатов
    outputPrecision = 5
}

main :: IO ()
main = putStrLn.prettyPrintOutputs =<< simulateManyBuffers baseInput
    [ (erlang2, [System erlang2 3, System erlang2 3, System erlang2 3])
    ] simulate  
    where
      erlang2 = generationDistr "erlang" [10, 2]