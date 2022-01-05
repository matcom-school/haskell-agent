{-# LANGUAGE BlockArguments #-}

module Ambient where

import Control.Monad (join, replicateM)
import Control.Monad.IO.Class
import Data.Matrix
import System.Random

type AllAmbient = Matrix (SpaceOfAmbient (Maybe MemberOfAmbient))

data SpaceOfAmbient a = Empty a | Corral a deriving (Show)

data MemberOfAmbient = Child | Robot | Dirt | Obstacles deriving (Show)

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

createAmbient :: (MonadIO m) => (Int, Int) -> Int -> Int -> m AllAmbient
createAmbient (n, m) childCount robotCount = do
  list <- getRandomList (n * m)
  childPosList <- getNNewRandomPosition (0, n * m) childCount []
  robotPosList <- getNNewRandomPosition (childCount + 1, n * m) robotCount childPosList
  let listWithOutRobotAndChild = mapIntToMemberOfAmbient list
  let listWithChildAndWithOutRobot = insertMemberToPosition childPosList (Just Child) listWithOutRobotAndChild
  let listWithChildAndRobot = insertMemberToPosition robotPosList (Just Robot) listWithChildAndWithOutRobot
  return $
    matrix n m $ \(i, j) ->
      let index = n * (i - 1) + j - 1
          member = listWithChildAndRobot !! index
       in if index < childCount
            then Corral member
            else Empty member
