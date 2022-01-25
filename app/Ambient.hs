module Ambient where

import Control.Monad (join, replicateM)
import Control.Monad.IO.Class
import Data.List
import Data.Matrix
import Data.Maybe
import System.Random
import TupleHelper

type AllAmbient = Matrix (SpaceOfAmbient, Maybe MemberOfAmbient)

data SpaceOfAmbient = Empty | Corral deriving (Show)

data MemberOfAmbient = Child | Robot | Dirt | Obstacles | RobotWithChild deriving (Show)

type IndexOfAmbient = (Int, SpaceOfAmbient, Maybe MemberOfAmbient)

isRobotWithChild :: IndexOfAmbient -> Bool
isRobotWithChild (_, _, Just RobotWithChild) = True
isRobotWithChild _ = False

isChild :: IndexOfAmbient -> Bool
isChild (_, _, Just Child) = True
isChild _ = False

isRobot :: IndexOfAmbient -> Bool
isRobot (_, _, Just Robot) = True
isRobot _ = False

isObstacles :: IndexOfAmbient -> Bool
isObstacles (_, Empty, Just Obstacles) = True
isObstacles _ = False

isDirt :: IndexOfAmbient -> Bool
isDirt (_, Empty, Just Dirt) = True
isDirt _ = False

isCorral :: IndexOfAmbient -> Bool
isCorral (_, Corral, _) = True
isCorral _ = False

isEmpty :: IndexOfAmbient -> Bool
isEmpty (_, Empty, _) = True
isEmpty _ = False

isFreePath :: IndexOfAmbient -> Bool
isFreePath x = isEmpty x || isDirt x

data Ambient = Ambient
  { getChildCount :: Int,
    getN :: Int,
    getM :: Int,
    getList :: [IndexOfAmbient]
  }
  deriving (Show)

getValues :: Ambient -> (Int, Int, [IndexOfAmbient])
getValues a = (getN a, getM a, getList a)

spaceList :: [Maybe MemberOfAmbient]
spaceList = [Just Dirt, Just Obstacles, Nothing]

spaceAccount :: Int
spaceAccount = length spaceList

mapIntToMemberOfAmbient :: [Int] -> [Maybe MemberOfAmbient]
mapIntToMemberOfAmbient = map (spaceList !!)

insertMemberToPosition :: [Int] -> Maybe MemberOfAmbient -> [Maybe MemberOfAmbient] -> [Maybe MemberOfAmbient]
insertMemberToPosition pos member = zipWith (curry (\x -> if fst x `elem` pos then member else snd x)) [0 ..]

getRandomList :: MonadIO m => Int -> m [Int]
getRandomList n = replicateM n $ randomRIO (0, spaceAccount - 1)

getNewRandomPosition :: MonadIO m => (Int, Int) -> [Int] -> m Int
getNewRandomPosition top list =
  do
    value <- randomRIO top
    let result
          | value `elem` list = getNewRandomPosition top list
          | otherwise = return value
    result

getNNewRandomPosition :: (MonadIO m) => (Int, Int) -> Int -> [Int] -> m [Int]
getNNewRandomPosition _ 0 _ = return []
getNNewRandomPosition top n list = do
  newValue <- getNewRandomPosition top list
  restList <- getNNewRandomPosition top (n - 1) (newValue : list)
  return (newValue : restList)

computeIndexInMatrixToList :: Num a => (a, a, c, a) -> a
computeIndexInMatrixToList (i, j, n, m) = m * i + j

ambientToMatrix :: Ambient -> AllAmbient
ambientToMatrix a =
  let n = getN a
      m = getM a
      list = map (\(i, s, m) -> (s, m)) $ getList a
   in matrix n m $ \(i, j) -> list !! computeIndexInMatrixToList (i -1, j -1, n, m)

createAmbient :: (MonadIO m) => (Int, Int) -> Int -> Int -> m Ambient
createAmbient (n, m) childCount robotCount = do
  list <- getRandomList (n * m)
  childPosList <- getNNewRandomPosition (0, n * m) childCount []
  robotPosList <- getNNewRandomPosition (childCount + 1, n * m) robotCount childPosList
  let listWithOutRobotAndChild = mapIntToMemberOfAmbient list
  let listWithChildAndWithOutRobot = insertMemberToPosition childPosList (Just Child) listWithOutRobotAndChild
  let listWithChildAndRobot = insertMemberToPosition robotPosList (Just Robot) listWithChildAndWithOutRobot
  let zipList = zip [0 ..] listWithChildAndRobot
  return $
    Ambient childCount n m $
      map
        ( \x ->
            let index = fst x
                member = snd x
             in if index < childCount
                  then (index, Corral, member)
                  else (index, Empty, member)
        )
        zipList

ambientMap :: (IndexOfAmbient -> b) -> Ambient -> [b]
ambientMap f a =
  let list = getList a
   in map f list

changeAmbient :: [(Int, Maybe MemberOfAmbient)] -> Ambient -> Ambient
changeAmbient changeList ambient = do
  let indexes = map fst changeList
  let members = map snd changeList
  let (n, m, list) = getValues ambient
  let f = (\(i, s, m) -> if i `elem` indexes then (i, s, members !! fromJust (elemIndex i indexes)) else (i, s, m))
  Ambient (getChildCount ambient) n m $ map f list

getIndexByIndex :: Int -> Ambient -> (Int, Int)
getIndexByIndex index a =
  let n = getN a
      m = getM a
      i = index `div` m
      j = index `mod` m
   in (i, j)

data Direction = N | NE | E | SE | S | SO | O | NO

directionList :: [Direction]
directionList = [N, NE, E, SE, S, SO, O, NO]

directionToTuple :: Direction -> (Int, Int)
directionToTuple d = case d of
  N -> (-1, 0)
  NE -> (-1, 1)
  E -> (0, 1)
  SE -> (1, 1)
  S -> (1, 0)
  SO -> (1, -1)
  O -> (0, -1)
  NO -> (-1, -1)

getAdjByDirection :: Int -> Ambient -> Direction -> Maybe IndexOfAmbient
getAdjByDirection index ambient direction =
  let (n, m, list) = getValues ambient
      (i, j) = getIndexByIndex index ambient
      (di, dj) = directionToTuple direction
      newI = i -1 + di
      newJ = j -1 + dj
   in if 0 <= newI && newI < n && 0 <= newJ && newJ < m
        then Just (list !! computeIndexInMatrixToList (newI, newJ, n, m))
        else Nothing

getAdjList :: Int -> Ambient -> [Maybe IndexOfAmbient]
getAdjList index a = map (getAdjByDirection index a) directionList

getQuadricycle :: Ambient -> Int -> [Int]
getQuadricycle ambient index = do
  let adjFunc = getAdjByDirection index ambient
  let adjList = map fst' $ mapMaybe adjFunc directionList
  index : adjList