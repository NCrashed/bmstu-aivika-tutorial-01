module Varying where

import Simulation.Aivika
import Control.Applicative

import Task

simulateManyBuffers :: Input -> [((String, Parameter Double), [System])] -> (Input -> Simulation Output) -> IO [Output]
simulateManyBuffers base buffs model = simulateMany (makeInput <$> buffs) model
  where 
    makeInput (lawGen, systems) = base { 
        inputSystems = systems
      , generationDistribution = lawGen
      }
        
simulateMany :: [Input] -> (Input -> Simulation Output) -> IO [Output]
simulateMany [] _ = return []
simulateMany inputs model = do
  mapM (\input -> runSimulation (model input) specs) inputs
  where
    specs = Specs {
      spcStartTime = 0.0,
      spcStopTime = simulationTime $ head inputs,
      spcDT = 10,
      spcMethod = RungeKutta4,
      spcGeneratorType = SimpleGenerator
    } 
  