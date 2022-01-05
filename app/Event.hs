module Event where

-- import Data.Matrix
-- import System.Random

-- m = matrix 4 4 $ \(i,j) -> 0
-- i = 1

get_hash :: String -> (Int, String)
get_hash s | s == "Par" = (1, "P")
           | otherwise = (2, "PP")
condition :: Int -> Bool
condition n = mod n 2 == 0


even_ :: Int -> String
even_ n | condition n = snd $ get_hash "Par" 
       | otherwise = snd $ get_hash "Impar" 

-- main = do
--     print "Hola"
--     print m
--     print $ zero 3 4 
--     print $ diagonalList 6 2 [1..] 
--     print $ permMatrix 3 1 2
--     print $ fromLists [[1,2], [3,4]]  
--     print $ getElem 1 2 $ fromLists [[1,2], [3,4]] 
--     print $ fromLists [[1,2], [3,4]] !(2,2) 
--     print $ setElem 30 (1, 2) $ fromLists [[1,2], [3,4]] 
--     print $ even_ 10
--     let e = setElem 30 (1, 2) m 
--     let a = 1
--     let m = e
--     print e
--     print m
--     randomIndex <- randomRIO (0, 2)
--     print $ [1,2,3] !! randomIndex
