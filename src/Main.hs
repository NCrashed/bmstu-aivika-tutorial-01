module Main where

import Task
import Model
import Util
import Tabular
import Varying

-- | Входные параметры модели
testInput :: Input
testInput = Input {
    -- | Закон распределения потока заявок
    -- Указывается один из: exponential, erlang, normal, uniform
    -- каждому закону нужно указать список параметров
    -- для exponential нужен 1 параметр (среднее время между заявками)
    -- для других (erlang и т.д.) нужно по 2 параметра
    generationDistribution = generationDistr "erlang" [10, 2],
    -- | Закон распределения обработки заявок в обслуживающем автомате
    -- параметры аналогичны.
    processingDistribution = generationDistr "erlang" [10, 2],
    -- | Емкость буфферов
    bufferCapacity = [3, 3, 3],
    -- | Время иммитационного моделирования
    simulationTime = 500000.0,
    -- | Количество знаков после запятой в выводе результатов
    outputPrecision = 5
}

main :: IO ()
main = putStrLn.prettyPrintOutputs =<< simulateManyBuffers testInput
    [ (generationDistr "erlang" [10, 2], generationDistr "erlang" [10, 2], [3, 3, 3])
    , (generationDistr "erlang" [10, 2], generationDistr "erlang" [10, 2], [4, 3, 3])
    , (generationDistr "erlang" [10, 2], generationDistr "erlang" [10, 2], [10, 3, 3])
    ] simulate  