module Ambient where

import Control.Monad (join, replicateM)
import Control.Monad.IO.Class
import Data.List
import Data.Matrix
import Data.Maybe
import System.Random
import TupleHelper

type AllAmbient = Matrix IndexOfAmbient

data SpaceOfAmbient = Clean | Dirt | Corral | Obstacles deriving (Show)

data MemberOfAmbient = Child | Robot | RobotWithChild deriving (Show)

type TupleOfAmbient = (SpaceOfAmbient, Maybe MemberOfAmbient)

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
isObstacles (_, Obstacles, _) = True
isObstacles _ = False

isDirt :: IndexOfAmbient -> Bool
isDirt (_, Dirt, _) = True
isDirt _ = False

isClean :: IndexOfAmbient -> Bool
isClean (_, Clean, _) = True
isClean _ = False

isCorral :: IndexOfAmbient -> Bool
isCorral (_, Corral, _) = True
isCorral _ = False

isBeEmpty :: IndexOfAmbient -> Bool
isBeEmpty index = not (isObstacles index) && (isNothing . last') index

data Ambient = Ambient
  { getChildCount :: Int,
    getN :: Int,
    getM :: Int,
    getList :: [IndexOfAmbient]
  }
  deriving (Show)

getValues :: Ambient -> (Int, Int, [IndexOfAmbient])
getValues a = (getN a, getM a, getList a)

spaceList :: [TupleOfAmbient]
spaceList = [(Obstacles, Nothing), (Dirt, Nothing), (Dirt, Nothing)]

spaceAccount :: Int
spaceAccount = length spaceList

mapIntToTupleOfAmbient :: [Int] -> [(SpaceOfAmbient, Maybe MemberOfAmbient)]
mapIntToTupleOfAmbient = map (spaceList !!)

insertTupleOfAmbient :: [Int] -> TupleOfAmbient -> [TupleOfAmbient] -> [TupleOfAmbient]
insertTupleOfAmbient pos member = zipWith (curry (\x -> if fst x `elem` pos then member else snd x)) [0 ..]

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
getNNewRandomPosition top n list = replicateM n $ getNewRandomPosition top list

computeIndexInMatrixToList :: Num a => (a, a, c, a) -> a
computeIndexInMatrixToList (i, j, n, m) = m * i + j

ambientToMatrix :: Ambient -> AllAmbient
ambientToMatrix a =
  let n = getN a
      m = getM a
      list = getList a
   in matrix n m $ \(i, j) -> list !! computeIndexInMatrixToList (i -1, j -1, n, m)

generateAmbient :: (MonadIO m) => (Int, Int) -> Int -> [IndexOfAmbient] -> m Ambient
generateAmbient (n, m) child childAndRobots = do
  nxmRandomNum <- getRandomList (n * m)
  let ambientWithOutChidAndRobot = zip [0 ..] $ mapIntToTupleOfAmbient nxmRandomNum
  return $ Ambient child n m $ map replaceChildAndRobots ambientWithOutChidAndRobot
  where
    replaceChildAndRobots (index, (space, member)) =
      let maybeChildOrRobot = find ((== index) . fst') childAndRobots
          realMember
            | index < child = Nothing
            | otherwise = member
          realSpace
            | index < child = Corral
            | isJust maybeChildOrRobot && isChild (fromJust maybeChildOrRobot) = Clean
            | isJust maybeChildOrRobot && isObstacles (1, space, Nothing) = Dirt
            | otherwise = space
       in case maybeChildOrRobot of
            Just (_, _, childOrRobot) -> (index, realSpace, childOrRobot)
            Nothing -> (index, realSpace, realMember)

createAmbient :: (MonadIO m) => (Int, Int) -> Int -> Int -> m Ambient
createAmbient (n, m) childCount robotCount = do
  childPosList <- getNNewRandomPosition (0, n * m - 1) childCount []
  robotPosList <- getNNewRandomPosition (childCount + 1, n * m -1) robotCount childPosList
  generateAmbient (n, m) childCount $ map childFunc childPosList ++ map robotFunc robotPosList
  where
    childFunc x = (x, Corral, Just Child)
    robotFunc x = (x, Clean, Just Robot)

setMemberInPlace :: Ambient -> Int -> Maybe MemberOfAmbient -> IndexOfAmbient
setMemberInPlace ambient index member
  | isObstacles (getList ambient !! index) && isChild (1, Clean, member) = (index, Clean, Just Child)
  | isObstacles (getList ambient !! index) && isJust member = head $ tail []
  | otherwise = f $ getList ambient !! index
  where
    f (index, space, m) = (index, space, member)

setSpaceInPlace :: Ambient -> Int -> SpaceOfAmbient -> IndexOfAmbient
setSpaceInPlace ambient index space = f $ getList ambient !! index
  where
    f (index, s, member) = (index, space, member)

ambientMap :: (IndexOfAmbient -> b) -> Ambient -> [b]
ambientMap f a =
  let list = getList a
   in map f list

changeAmbient :: [IndexOfAmbient] -> Ambient -> Ambient
changeAmbient changeList ambient = do
  let indexes = map fst' changeList
  let (n, m, list) = getValues ambient
  let f = (\(i, s, m) -> if i `elem` indexes then changeList !! fromJust (elemIndex i indexes) else (i, s, m))
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
      newI = i + di
      newJ = j + dj
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