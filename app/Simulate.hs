module Simulate where

import Agent
import Ambient
import Child
import Control.Monad.IO.Class
import Data.Maybe
import Statistics
import TupleHelper (fst')

simulate :: (MonadIO m) => (Int, Int) -> (Int, Int) -> Int -> Int -> m (Maybe Int)
simulate (n, m) (childAccount, robotAccount) t totalTime = do
  ambient <- createAmbient (n, m) childAccount robotAccount -- get e0
  historyResult <- run ambient 0 t totalTime -- run with ambient t and total get history
  let listDirtPercent = dirtPercent historyResult
  return $ if not $ any (> 60) listDirtPercent then Just $ median listDirtPercent else Nothing -- value history
  where
    dirtPercent history =
      let binaryMapDirty = map (\x -> if isDirt x then 1 else 0)
       in map ((* 100) . median . binaryMapDirty . getList) history

run :: (MonadIO m) => Ambient -> Int -> Int -> Int -> m [Ambient]
run actualAmbient actualTime tFactor tTotal
  | actualTime == tTotal = return []
  | (actualTime `mod` tFactor) /= 0 = actualizeByAgent actualAmbient actualTime tFactor tTotal
  | otherwise = do
    newAmbient <- actualizeByAmbientNaturalChange actualAmbient
    actualizeByAgent newAmbient actualTime tFactor tTotal
  where
    actualizeByAmbientNaturalChange ambient = randomChangeAmbient ambient
    actualizeByAgent ambient actualTime tFactor tTotal = do
      let newAmbient = movAllAgents ambient
      restAmbients <- run newAmbient (actualTime + 1) tFactor tTotal
      return (newAmbient : restAmbients)

movAllAgents :: Ambient -> Ambient
movAllAgents ambient = do
  let robotWithChildList = map fst' $ filter isRobotWithChild $ getList ambient
  let movRobotWithChild = concatMap (robotWithChildMove ambient) robotWithChildList
  let robotList = map fst' $ filter isRobot $ getList ambient
  let perceptionList = zip robotList $ map (see ambient) robotList
  let socializeDecision = socialize ambient perceptionList
  -- execute
  changeAmbient (movRobotWithChild) ambient

randomChangeAmbient :: (MonadIO m) => Ambient -> m Ambient
randomChangeAmbient ambient = do
  let childIndex = map fst' $ filter isChild (getList ambient)
  actionList <- getActionList ambient childIndex
  executeAction ambient actionList
  where
    getActionList ambient [] = return [Nothing]
    getActionList ambient (x : rest) = do
      action <- getChildAction ambient x
      restAction <- getActionList ambient rest
      return (action : restAction)

    executeAction ambient [] = return ambient
    executeAction ambient (x : rest)
      | isNothing x = executeAction ambient rest
      | otherwise = do
        newAmbient <- executeChildAction (fromJust x) ambient
        executeAction newAmbient rest