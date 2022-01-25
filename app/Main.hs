module Main where

import Data.Set hiding (filter, map)
import Simulate

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
  | otherwise = print $ length lastResult
  where
    runSimulation index last =
      do
        newResult <- simulate (heightAmbient, widthAmbient) (childInAmbient, robotInAmbient) changeFactorOfAmbient timeOfSimulation
        case newResult of
          Just value -> doSimulation (index + 1) (value : last)
          Nothing -> doSimulation (index + 1) (-1 : last)
    variance listResult = 0.3

main :: IO ()
main = print $ fromList [1, 2, 3] == fromList [2, 1, 3]

-- Time Helper
hourToMin :: Int -> Int
hourToMin hours = 60 * hours

dayToMin :: Int -> Int
dayToMin days = hourToMin (days * 24)
