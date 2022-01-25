module Child where

import Algorithm.Search
import Ambient hiding (getIndex)
import Control.Monad (forM_, join)
import Control.Monad.IO.Class
import Data.Maybe
import Data.Set hiding (filter, map)
import System.Random
import TupleHelper

data Action = Action {getIndex :: Int, lastIndex :: Int, getDirection :: Direction}

childDirectionCondition :: Ambient -> Int -> Direction -> Maybe Action
childDirectionCondition ambient index dir = do
  let maybeIndexOfAmbient = getAdjByDirection index ambient dir
  case maybeIndexOfAmbient of
    Just indexOfAmbient -> if canMovChildTo indexOfAmbient dir ambient then Just $ Action (fst' indexOfAmbient) index dir else Nothing
    Nothing -> Nothing

canMovChildTo :: IndexOfAmbient -> Direction -> Ambient -> Bool
canMovChildTo (_, _, Nothing) _ _ = True
canMovChildTo (index, _, Just Obstacles) dir ambient = canMovObstacles index dir ambient
canMovChildTo _ _ _ = False

canMovObstacles :: Int -> Direction -> Ambient -> Bool
canMovObstacles index dir ambient = do
  let maybeIndexOfAmbient = getAdjByDirection index ambient dir
  case maybeIndexOfAmbient of
    Nothing -> False
    Just (_, Empty, Nothing) -> True
    Just (_, Empty, Just Dirt) -> True
    Just (nextIndex, Empty, Just Obstacles) -> canMovObstacles nextIndex dir ambient
    Just _ -> False

getChildAction :: (MonadIO m) => Ambient -> Int -> m (Maybe Action)
getChildAction ambient index =
  do
    doMove <- randomRIO (0, 1 :: Int)
    let actionMap = childDirectionCondition ambient index
    let indexOfAmbient = getList ambient !! index
    let actionList = if isChild indexOfAmbient && not (isCorral indexOfAmbient) then mapMaybe actionMap directionList else []
    let lenOfActionList = length actionList
    let result
          | doMove == 1 || lenOfActionList == 0 = return Nothing
          | otherwise = do
            randomIndex <- randomRIO (0, length actionList)
            return $ Just $ actionList !! randomIndex
    result

executeChildAction :: (MonadIO m) => Action -> Ambient -> m Ambient
executeChildAction action ambient
  | not (isChild $ getList ambient !! getIndex action) = return ambient
  | otherwise = do
    let index = getIndex action
    let placeOfObstacles = case getList ambient !! index of
          (_, _, Just Obstacles) -> findPlaceOfObstaclesByDirection ambient index $ getDirection action
          _ -> []
    let centralQuadricycle = getQuadricycle ambient index
    placeOfNewDirt <- newDirt centralQuadricycle []
    return $ changeAmbient ([(index, Just Child), (lastIndex action, Nothing)] ++ placeOfObstacles ++ placeOfNewDirt) ambient
  where
    newDirt [] result = return result
    newDirt (index : indexes) result = do
      newPosition <- generateDirt ambient $ getQuadricycle ambient index
      newDirt indexes $ result ++ newPosition

findPlaceOfObstaclesByDirection :: Ambient -> Int -> Direction -> [(Int, Maybe MemberOfAmbient)]
findPlaceOfObstaclesByDirection ambient index dir = do
  let adjPost = getAdjByDirection index ambient dir
  case adjPost of
    Nothing -> []
    Just (nextIndex, Empty, Nothing) -> [(nextIndex, Just Obstacles)]
    Just (nextIndex, Empty, Just Dirt) -> [(nextIndex, Just Obstacles)]
    Just (nextIndex, Empty, Just Obstacles) -> findPlaceOfObstaclesByDirection ambient nextIndex dir
    Just _ -> []

generateDirt :: (MonadIO m) => Ambient -> [Int] -> m [(Int, Maybe MemberOfAmbient)]
generateDirt ambient quadricycle = do
  let quadricycleInAmbient = map (getList ambient !!) quadricycle
  let countChild = length $ filter isChild quadricycleInAmbient
  let freeSpace = filter (not . isEmpty) quadricycleInAmbient
  firstRandom <- randomRIO (0, length freeSpace)
  secondRandom <- randomRIO (0, length freeSpace)
  threeRandom <- randomRIO (0, length freeSpace)
  case countChild of
    1 -> return $ generateDirtByRandom freeSpace [firstRandom]
    2 -> return $ generateDirtByRandom freeSpace [firstRandom, secondRandom]
    _ -> return $ generateDirtByRandom freeSpace [firstRandom, secondRandom, threeRandom]
  where
    generateDirtByRandom freeSpace randomList = do
      let f = \x -> (fst' $ freeSpace !! x, Just Dirt)
      map f randomList
