module Main where

import Agent
import Algorithm.Search
import Ambient
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Maybe
import Data.Ord
import Data.Set hiding (filter, map)
import Simulate
import Statistics
import System.IO.Unsafe
import System.Random
import TupleHelper

heightAmbient :: Int
heightAmbient = 3

widthAmbient :: Int
widthAmbient = 3

childInAmbient :: Int
childInAmbient = 2

robotInAmbient :: Int
robotInAmbient = 2

changeFactorOfAmbient :: Int
changeFactorOfAmbient = hourToMin 1

timeOfSimulation :: Int
timeOfSimulation = dayToMin 1

doSimulation :: (Show a2, Real a2, Ord t, Fractional a2, Num t) => t -> [a2] -> IO ()
doSimulation simulationIndex lastResult
  | simulationIndex < 1000 = do
    print $ variance lastResult
    runSimulation simulationIndex lastResult
  | otherwise = do
    print "Finish Simulation"
    print $ median lastResult
  where
    runSimulation index last =
      do
        newResult <- simulate (heightAmbient, widthAmbient) (childInAmbient, robotInAmbient) changeFactorOfAmbient timeOfSimulation
        doSimulation (index + 1) (newResult : last)

main :: IO ()
main = doSimulation 0 []

-- Time Helper
hourToMin :: Int -> Int
hourToMin hours = 60 * hours

dayToMin :: Int -> Int
dayToMin days = hourToMin (days * 24)
