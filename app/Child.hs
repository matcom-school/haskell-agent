module Child where

import Algorithm.Search
import Ambient hiding (getIndex)
import Control.Monad (forM, forM_, join)
import Control.Monad.IO.Class
import Data.Maybe
import Data.Set hiding (filter, map)
import System.Random
import TupleHelper

data Action = Action
  { getIndex :: Int,
    lastIndex :: Int,
    getDirection :: Direction
  }

childDirectionCondition :: Ambient -> Int -> Direction -> Maybe Action
childDirectionCondition ambient index dir = do
  let maybeIndexOfAmbient = getAdjByDirection index ambient dir
  case maybeIndexOfAmbient of
    Just indexOfAmbient ->
      if canMovChildTo indexOfAmbient dir ambient
        then Just $ Action (fst' indexOfAmbient) index dir
        else Nothing
    Nothing -> Nothing

canMovChildTo :: IndexOfAmbient -> Direction -> Ambient -> Bool
canMovChildTo (index, Obstacles, Nothing) dir ambient = canMovObstacles index dir ambient
canMovChildTo index _ _ = (isCorral index || isClean index) && isBeEmpty index

canMovObstacles :: Int -> Direction -> Ambient -> Bool
canMovObstacles index dir ambient = do
  let maybeIndexOfAmbient = getAdjByDirection index ambient dir
  case maybeIndexOfAmbient of
    Just (nextIndex, Obstacles, Nothing) -> canMovObstacles nextIndex dir ambient
    Just (nextIndex, space, Nothing) -> not $ isCorral (nextIndex, space, Nothing)
    _ -> False

getChildAction :: (MonadIO m) => Ambient -> Int -> m (Maybe Action)
getChildAction ambient index =
  do
    doMove <- randomRIO (0, 1 :: Int)
    let actionMap = childDirectionCondition ambient index
    let actionList = mapMaybe actionMap directionList
    let lenOfActionList = length actionList
    let result
          | doMove == 1 || lenOfActionList == 0 = return Nothing
          | otherwise = do
            randomIndex <- randomRIO (0, length actionList - 1)
            return $ Just $ actionList !! randomIndex
    result

executeChildAction :: (MonadIO m) => Ambient -> Action -> m Ambient
executeChildAction ambient action
  | not (isChild $ getList ambient !! lastIndex action) = return ambient
  | otherwise = do
    let index = getIndex action
    let setFunc = setMemberInPlace ambient
    let placeOfObstacles
          | isObstacles $ getList ambient !! index = findPlaceOfObstaclesByDirection ambient index $ getDirection action
          | otherwise = []
    let centralQuadricycle = getQuadricycle ambient $ lastIndex action
    placeOfNewDirt <- newDirt centralQuadricycle []
    return $ changeAmbient ([setFunc index (Just Child), setFunc (lastIndex action) Nothing] ++ placeOfObstacles ++ placeOfNewDirt) ambient
  where
    newDirt [] result = return result
    newDirt (index : indexes) result = do
      newPosition <- generateDirt ambient $ getQuadricycle ambient index
      newDirt indexes $ result ++ map (\x -> setSpaceInPlace ambient x Dirt) newPosition

findPlaceOfObstaclesByDirection :: Ambient -> Int -> Direction -> [IndexOfAmbient]
findPlaceOfObstaclesByDirection ambient index dir = do
  let adjPost = getAdjByDirection index ambient dir
  case adjPost of
    Just (nextIndex, Obstacles, Nothing) -> findPlaceOfObstaclesByDirection ambient nextIndex dir
    Just (nextIndex, space, Nothing) -> [(nextIndex, Obstacles, Nothing) | not (isCorral (nextIndex, space, Nothing))]
    _ -> []

generateDirt :: (MonadIO m) => Ambient -> [Int] -> m [Int]
generateDirt ambient quadricycle = do
  let quadricycleInAmbient = map (getList ambient !!) quadricycle
  let countChild = length $ filter isChild quadricycleInAmbient
  let freeSpace = filter (\x -> not (isCorral x) && isBeEmpty x) quadricycleInAmbient
  firstRandom <- randomRIO (0, length freeSpace - 1)
  secondRandom <- randomRIO (0, length freeSpace - 1)
  threeRandom <- randomRIO (0, length freeSpace - 1)
  case countChild of
    1 -> return $ generateDirtByRandom freeSpace [firstRandom]
    2 -> return $ generateDirtByRandom freeSpace [firstRandom, secondRandom]
    _ -> return $ generateDirtByRandom freeSpace [firstRandom, secondRandom, threeRandom]
  where
    generateDirtByRandom freeSpace randomList
      | Prelude.null freeSpace = []
      | otherwise = map (\x -> fst' $ freeSpace !! x) randomList
