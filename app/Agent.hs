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
       in map fst $ sortOn (Down . fst . snd) $ zip (map fst' childList) resultList

analyzeChild :: Ambient -> Int -> (Int -> Maybe (Int, [Int]))
analyzeChild ambient index =
  let childList = getChildList ambient
      resultList = map (dijkstraAnalyze . fst') childList
   in fResult childList resultList
  where
    dijkstraAnalyze target = dijkstra adjFunc costFunc (== target) index
    adjFunc current = map fst' $ filter isFreePath $ catMaybes $ getAdjList current ambient
    costFunc current next
      | isDirt (getList ambient !! next) = 1
      | otherwise = 2

    fResult childList resultList child =
      snd =<< find ((== child) . fst) (zip (map fst' childList) resultList)

adjDirtPath :: Ambient -> Int -> Int
adjDirtPath ambient index =
  let adjList = map fst' $ catMaybes $ getAdjList index ambient
      dirtValue = map (dfsDirt ambient) adjList
   in fst $ maximumBy (comparing snd) $ zip adjList dirtValue
  where
    dfsDirt ambient index
      | not $ isDirt $ getList ambient !! index = 0
      | otherwise =
        let results = map ((+ 1) . dfsDirt ambient . fst') $ catMaybes $ getAdjList index ambient
         in maximumBy (comparing id) results

-- True if b perception is better of a
comparePerceptions :: Int -> Perception -> Perception -> Bool
comparePerceptions index a b =
  case (getBestPathTo a index, getBestPathTo b index) of
    (Just aPerception, Nothing) -> False
    (Nothing, _) -> True
    (Just aPerception, Just bPerception) -> (fst aPerception `div` length (snd aPerception)) >= (fst bPerception `div` length (snd bPerception))

socialize :: Ambient -> [(Int, Perception)] -> [(Int, Int)]
socialize ambient robotPerception = run []
  where
    step = head robotPerception
    possibleRoute selected =
      let childList = map fst' $ getChildList ambient
       in filter (\x -> isNothing $ find ((== x) . snd) selected) childList
    run robotSelect
      | length robotPerception == length robotSelect = []
      | null $ possibleRoute robotSelect = run $ second getAlternativeRoute step : robotSelect
      | otherwise = do
        let childFree = possibleRoute robotSelect
        let childPriority = filter (\x -> isNothing $ find (== x) childFree) $ selectionOrd $ snd step
        let otherPerceptions = map snd $ filter (\x -> isNothing $ find ((== fst x) . fst) robotSelect) robotPerception
        let stepPerception = snd step
        case find (\x -> isNothing $ find (comparePerceptions x stepPerception) otherPerceptions) childPriority of
          Just value -> run $ (fst step, head $ snd $ fromJust $ getBestPathTo (snd step) value) : robotSelect
          Nothing -> run $ second getAlternativeRoute step : robotSelect

executeSocialDecision :: [(Int, Int)] -> [(Int, Maybe MemberOfAmbient)]
executeSocialDecision = concatMap f
  where
    f (x, y) = [(x, Nothing), (y, Just Robot)]

robotWithChildMove :: Ambient -> Int -> [(Int, Maybe MemberOfAmbient)]
robotWithChildMove ambient index = do
  let indexOfAmbient = getList ambient !! index
  if condition indexOfAmbient then tryUnchanged else mov
  where
    condition index = isCorral index && isEmpty index
    tryUnchanged =
      let emptySpaces = filter isEmpty $ catMaybes $ getAdjList index ambient
       in if null emptySpaces then [] else [(index, Just Child), (fst' $ head emptySpaces, Just Robot)]

    mov =
      let path = bfs adj (isCorral . (getList ambient !!)) index
       in case path of
            Just steps -> [(index, Nothing), (head steps, Just Robot)]
            Nothing -> []
    adj index = map fst' $ filter isFreePath $ catMaybes $ getAdjList index ambient
