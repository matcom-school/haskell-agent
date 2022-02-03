module Main where

import Agent
import Ambient
import Data.Foldable
import Data.Maybe
import Data.Set hiding (filter, map)
import Simulate
import Statistics
import TupleHelper

heightAmbient :: Int
heightAmbient = 3

widthAmbient :: Int
widthAmbient = 3

childInAmbient :: Int
childInAmbient = 3

robotInAmbient :: Int
robotInAmbient = 3

changeFactorOfAmbient :: Int
changeFactorOfAmbient = hourToMin 1

timeOfSimulation :: Int
timeOfSimulation = dayToMin 1

doSimulation :: Int -> [Int] -> IO ()
doSimulation simulationIndex lastResult
  | simulationIndex < 30 || variance lastResult > 0.5 = runSimulation simulationIndex lastResult
  | otherwise = print $ median lastResult
  where
    runSimulation index last =
      do
        newResult <- simulate (heightAmbient, widthAmbient) (childInAmbient, robotInAmbient) changeFactorOfAmbient timeOfSimulation
        doSimulation (index + 1) (newResult : last)
    variance listResult = 0.3

e :: [Int]
e = []

main :: IO ()
main = do
  -- ambient <- generateAmbient (5, 5) 2 [(1, Corral, Just Child)]
  -- print $ ambientToMatrix ambient

  a1 <- createAmbient (5, 5) 2 2
  print $ ambientToMatrix a1
  let a2 = changeAmbient [(0, Corral, Just Child)] a1
  print $ ambientToMatrix a2

-- let a2 = movAllAgents a1
-- print $ ambientToMatrix a2
-- let a3 = movAllAgents a2
-- print $ ambientToMatrix a3
-- let a4 = movAllAgents a3
-- print $ ambientToMatrix a4
-- let a5 = movAllAgents a4
-- print $ ambientToMatrix a5
-- let a6 = movAllAgents a5
-- print $ ambientToMatrix a6
-- let a7 = movAllAgents a6
-- print $ ambientToMatrix a7

-- let a = map fst' $ filter isRobot $ getList a1
-- let child = map fst' $ filter isChild $ getList a1

-- let robotList = map fst' $ filter isRobot $ getList a1
-- let perceptionList = zip robotList $ map (see a1) robotList
-- let socializeDecision = socialize a1 perceptionList
-- print $ map (getAlternativeRoute . snd) perceptionList
-- print $ map (selectionOrd . snd) perceptionList
-- print $ map (\x -> map (getBestPathTo $ snd x) child) perceptionList
-- print socializeDecision
-- print $ filter (not . (`elem` [1])) [1, 2, 3]
-- print "finish"

-- Time Helper
hourToMin :: Int -> Int
hourToMin hours = 60 * hours

dayToMin :: Int -> Int
dayToMin days = hourToMin (days * 24)
