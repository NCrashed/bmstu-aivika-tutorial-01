module Main where

import Simulation.Aivika
import Task
import Model

testInput :: Input
testInput = Input {
    generationTime = 10,
    processingTime = 50,
    bufferCapacity = 2
}

main :: IO ()
main = print =<< runSimulation (runEventInStartTime $ simulateProcess testInput) specs
    where specs = Specs {
              spcStartTime = 0.0,
              spcStopTime = 1000.0,
              spcDT = 10,
              spcMethod = RungeKutta4,
              spcGeneratorType = SimpleGenerator
          }   