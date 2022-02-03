module Agent where

import Algorithm.Search
import Ambient
import Data.Bifunctor (second)
import Data.List (elemIndex, find, maximumBy, sortBy, sortOn)
import Data.Maybe
import Data.Ord (Down (Down), comparing)
import TupleHelper (fst')

data Perception = Perception
  { getIndex :: Int,
    getBestPathTo :: Int -> Maybe (Int, [Int]),
    selectionOrd :: [Int],
    getAlternativeRoute :: Int
  }

isFreePathByAgent :: IndexOfAmbient -> Bool
isFreePathByAgent x = isBeEmpty x || isDirt x

getChildList :: Ambient -> [IndexOfAmbient]
getChildList ambient = filter (\x -> isChild x && not (isCorral x)) $ getList ambient

see :: Ambient -> Int -> Perception
see ambient index =
  let funcPathByChild = analyzeChild ambient index
      alternativeRoute = adjDirtPath ambient index
   in Perception index funcPathByChild (ordPath funcPathByChild) alternativeRoute
  where
    ordPath f =
      let childList = getChildList ambient
          resultList = mapMaybe (f . fst') childList
          pathToCorralByIndex = pathToCorral ambient
          realResult = filter (isJust . pathToCorralByIndex . last . snd) resultList
       in map fst $ sortOn (fst . snd) $ zip (map fst' childList) resultList

analyzeChild :: (Num cost, Ord cost) => Ambient -> Int -> Int -> Maybe (cost, [Int])
analyzeChild ambient index =
  let childList = getChildList ambient
      resultList = map (dijkstraAnalyze . fst') childList
   in fResult childList resultList
  where
    dijkstraAnalyze target = dijkstra (adjFunc target) costFunc ((== target) . fst) (index, [index])
    adjFunc target current =
      let condition = (\x -> ((fst' x == target) || isFreePathByAgent x) && notElem (fst' x) (snd current))
          adjList = catMaybes $ getAdjList (fst current) ambient
       in map (\x -> (fst' x, fst' x : snd current)) $ filter condition adjList
    costFunc current next
      | isDirt (getList ambient !! fst next) = 9
      | otherwise = 10

    fResult childList resultList child = do
      let realResultList = catMaybes resultList
      let result = find ((== child) . fst) (zip (map fst' childList) realResultList)
      case result of
        Nothing -> Nothing
        Just value -> Just $ second (map fst) $ snd value

adjDirtPath :: Ambient -> Int -> Int
adjDirtPath ambient index =
  let adjList = map fst' $ filter isFreePathByAgent $ catMaybes $ getAdjList index ambient
      dirtValue = map (dfsDirt ambient [index]) adjList
   in if null dirtValue then index else fst $ maximumBy (comparing snd) $ zip adjList dirtValue
  where
    dfsDirt ambient visited index
      | index `elem` visited = -1
      | not $ isDirt $ getList ambient !! index = 0
      | otherwise =
        let adjList = filter isFreePathByAgent $ catMaybes $ getAdjList index ambient
            results = map ((+ 1) . dfsDirt ambient (index : visited) . fst') adjList
         in if null results then -1000 else maximumBy (comparing id) results

-- True if b perception is better of a
comparePerceptions :: Int -> Perception -> Perception -> Bool
comparePerceptions index a b =
  case (getBestPathTo a index, getBestPathTo b index) of
    (Just aPerception, Nothing) -> False
    (Nothing, _) -> True
    (Just aPerception, Just bPerception) -> length (snd aPerception) > length (snd bPerception)

socialize :: Ambient -> [(Int, Perception)] -> [(Int, Int)]
socialize ambient robotPerception = run robotPerception []
  where
    possibleRoute selected =
      let childList = map fst' $ getChildList ambient
       in filter (not . (`elem` map snd selected)) childList

    selectBy index condition action
      | condition $ getList ambient !! index = action
      | otherwise = (index, index)

    tryMovToAlternativeRoute perception = selectBy (fst perception) (not . isDirt) (second getAlternativeRoute perception)
    tryMovToChildDirection perception childIndex =
      let path = snd $ fromJust $ getBestPathTo (snd perception) childIndex
       in selectBy (fst perception) (\_ -> length path < 3) (second (const (head path)) perception)

    run [] selected = selected
    run (perception : rest) selected
      | null $ possibleRoute selected = run rest $ tryMovToAlternativeRoute perception : selected
      | otherwise = do
        let childFree = possibleRoute selected
        let childPriority = filter (`elem` childFree) $ selectionOrd $ snd perception
        let otherPerceptions = map snd rest
        let stepPerception = snd perception
        case find (\x -> isNothing $ find (comparePerceptions x stepPerception) otherPerceptions) childPriority of
          Just value -> run rest $ tryMovToChildDirection perception value : selected
          Nothing -> run rest $ tryMovToAlternativeRoute perception : selected

executeSocialDecision :: Ambient -> [(Int, Int)] -> [IndexOfAmbient]
executeSocialDecision ambient = concatMap f
  where
    f (x, y)
      | x == y = [(x, Clean, Just Robot)]
      | isChild $ getList ambient !! y = [setMemberInPlace ambient x Nothing, setMemberInPlace ambient y (Just RobotWithChild)]
      | otherwise = [setMemberInPlace ambient x Nothing, setMemberInPlace ambient y (Just Robot)]

robotWithChildMove :: Ambient -> Int -> [IndexOfAmbient]
robotWithChildMove ambient index = do
  let indexOfAmbient = getList ambient !! index
  if condition indexOfAmbient then tryUnchanged else mov
  where
    emptySpaces index = filter isFreePathByAgent $ catMaybes $ getAdjList index ambient
    condition index = isCorral index && isBeEmpty index && not (null $ emptySpaces $ fst' index)
    tryUnchanged =
      [ (index, Corral, Just Child),
        setMemberInPlace ambient (fst' $ head $ emptySpaces index) (Just Robot)
      ]
    mov =
      let path = pathToCorral ambient index
       in case path of
            Just steps ->
              let step = if length steps >= 2 then head $ tail steps else head steps
               in [setMemberInPlace ambient index Nothing, setMemberInPlace ambient step $ Just RobotWithChild]
            Nothing ->
              let adjFree = emptySpaces index
               in leaveChild (getList ambient !! index) adjFree

    leaveChild robot adjFree =
      let dirtPlace = find (\x -> isDirt x && isBeEmpty x) adjFree
          cleanPlace = find (\x -> isClean x && isBeEmpty x) adjFree
          result
            | isClean robot && isJust dirtPlace = [setMemberInPlace ambient (fst' robot) $ Just Child, setMemberInPlace ambient (fst' $ fromJust dirtPlace) $ Just Robot]
            | isClean robot && isJust cleanPlace = [setMemberInPlace ambient (fst' robot) $ Just Child, setMemberInPlace ambient (fst' $ fromJust dirtPlace) $ Just Robot]
            | isDirt robot && isJust cleanPlace = [setMemberInPlace ambient (fst' robot) $ Just Robot, setMemberInPlace ambient (fst' $ fromJust dirtPlace) $ Just Child]
            | otherwise = []
       in result

pathToCorral :: Ambient -> Int -> Maybe [Int]
pathToCorral ambient index =
  let result = bfs adj (condition . (getList ambient !!) . fst) (index, [index])
   in case result of
        Nothing -> Nothing
        Just value -> Just $ map fst value
  where
    condition x = isCorral x && isBeEmpty x
    adj index = map (\x -> (fst' x, fst' x : snd index)) $ filter isFreePathByAgent $ catMaybes $ getAdjList (fst index) ambient
