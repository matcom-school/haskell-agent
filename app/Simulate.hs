module Simulate where

import Agent
import Ambient
import Child
import Control.Monad
import Control.Monad.IO.Class
import Data.Maybe
import Statistics
import TupleHelper (fst')

dirtPercent :: Fractional b => [Ambient] -> [b]
dirtPercent history =
  let binaryMapDirty = map (\x -> if isDirt x then 1 else 0)
   in map ((* 100) . median . binaryMapDirty . getList) history

simulate :: (MonadIO m, Fractional b) => (Int, Int) -> (Int, Int) -> Int -> Int -> m b
simulate (n, m) (childAccount, robotAccount) t totalTime = do
  ambient <- createAmbient (n, m) childAccount robotAccount -- get e0
  historyResult <- run ambient 0 t totalTime -- run with ambient t and total get history
  let listDirtPercent = dirtPercent historyResult
  return $ median listDirtPercent -- value history

run :: (MonadIO m) => Ambient -> Int -> Int -> Int -> m [Ambient]
run actualAmbient actualTime tFactor tTotal
  | actualTime >= tTotal = return []
  | otherwise = do
    ambient <- getAmbient
    let ambientWithMovAgent = movAllAgents ambient
    ambientWithMovAgentAndNaturalChange <- randomChangeAmbient ambientWithMovAgent
    runAux ambientWithMovAgentAndNaturalChange
  where
    getAmbient
      | actualTime `mod` tFactor /= 0 = return actualAmbient
      | otherwise = do
        let (n, m, list) = getValues actualAmbient
        let childAndRobot = filter (\x -> isRobot x || isRobotWithChild x) list
        let childList = filter isChild list
        childPosList <- getNNewRandomPosition (0, n * m - 1) (length childList) $ map fst' childAndRobot
        generateAmbient (n, m) (getChildCount actualAmbient) (childAndRobot ++ map (\x -> (x, Clean, Just Child)) childPosList)

    runAux nextAmbient = do
      history <- run nextAmbient (actualTime + 1) tFactor tTotal
      return (actualAmbient : history)

movAllAgents :: Ambient -> Ambient
movAllAgents ambient = do
  let robotWithChildList = map fst' $ filter isRobotWithChild $ getList ambient
  let movRobotWithChild = concatMap (robotWithChildMove ambient) robotWithChildList
  let robotList = map fst' $ filter isRobot $ getList ambient
  let perceptionList = zip robotList $ map (see ambient) robotList
  let socializeDecision = socialize ambient perceptionList
  changeAmbient (movRobotWithChild ++ executeSocialDecision ambient socializeDecision) ambient

randomChangeAmbient :: (MonadIO m) => Ambient -> m Ambient
randomChangeAmbient ambient = do
  let childIndex = map fst' $ filter (\x -> isChild x && not (isCorral x)) (getList ambient)
  actionList <- forM childIndex (getChildAction ambient)
  foldM executeChildAction ambient $ catMaybes actionList
