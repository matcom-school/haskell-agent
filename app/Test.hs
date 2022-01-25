module Test where

import Algorithm.Search
import Ambient
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Foldable
import Data.List
import Data.Matrix
import Event
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.Random
import Text.Printf

x :: Int
x = 3

biggestInt, smallest :: Int
biggestInt = maxBound
smallest = minBound

n :: Integer
n = 123456789012345678901234567891234567891234567

reallyBig :: Int
reallyBig = 2 ^ 2 ^ 2 ^ 2 ^ 2

numDigits :: Int
numDigits = length (show reallyBig)

d1, d2 :: Double
d1 = 4.54567
d2 = 6.4567e-4

mySum :: Integer -> Integer
mySum 0 = 0
mySum n = n + mySum (n -1)

hailstone :: Integer -> Integer
hailstone n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

isEven :: Integer -> Bool
isEven n
  | even n = True
  | otherwise = False

p :: (Int, Char)
p = (3, 'x')

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y

sumThree :: Int -> Int -> Int -> Int
sumThree x y z = x + y + z

numbers, range, range2 :: [Integer]
numbers = [1, 2, 3, 4]
range = [1 .. 100]
range2 = [2, 4 .. 100]

emptyList = []

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (n -1)

-- initListLength :: [Integer] -> Integer
-- initListLength [] = 0
-- initListLength (x : xs) = 1 + initListLength xs

initListLength :: [Integer] -> Integer
initListLength = foldr (\x -> (+) 1) 0

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []
sumEveryTwo [x] = [x]
sumEveryTwo (x : y : zs) = (x + y) : sumEveryTwo zs

data Thing
  = Shoe
  | Ship
  | SealingWax
  | Cabbage
  | King
  deriving (Show)

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, Ship, SealingWax, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Shoe = True
isSmall Ship = False
isSmall SealingWax = True
isSmall Cabbage = True
isSmall King = False

data FailleDouble = Faille | Ok Double

safeDiv :: Double -> Double -> FailleDouble
safeDiv _ 0 = Faille
safeDiv x y = Ok (x / y)

okOrZero :: FailleDouble -> Double
okOrZero Faille = 0
okOrZero (Ok d) = d

type Name = String

data Employee = Employee
  { name :: Name,
    phone :: String
  }
  deriving (Show)

m_name1, m_name2 :: Maybe Name
m_name1 = Nothing
m_name2 = Just "Brent"

m_phone1, m_phone2 :: Maybe String
m_phone1 = Nothing
m_phone2 = Just "555-1234"

ex01 = Employee <$> m_name1 <*> m_phone1

ex02 = Employee <$> m_name1 <*> m_phone2

ex03 = Employee <$> m_name2 <*> m_phone1

ex04 = Employee <$> m_name2 <*> m_phone2

(.+) = liftA2 (+) -- addition lifted to some Applicative context

(.*) = liftA2 (*) -- same for multiplication

-- nondeterministic arithmetic
k = ([4, 5] .* pure 2) .+ [6, 1] -- (either 4 or 5) times 2, plus either 6 or 1

-- and some possibly-failing arithmetic too, just for fun

-- import System.Console.ParseArgs

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x + 1, x + 2]

ex05 = [10, 20, 30] >>= addOneOrTwo

-- m = matrix 4 4 $ \(i,j) -> do
--     let a = [ x | x <- (randomRIO (0, 2))]
--     let e = a !! 0
--     e
-- i = 1

-- get_hash :: String -> (Int, String)
-- get_hash s | s == "Par" = (1, "P")
--            | otherwise = (2, "PP")
-- condition :: Int -> Bool
-- condition n = mod n 2 == 0

-- even_ :: Int -> String
-- even_ n | condition n = snd $ get_hash "Par"
--        | otherwise = snd $ get_hash "Impar"

countChange target =
  dijkstra (add_coin `pruning` (> target)) true_cost (== target) 0
  where
    coin_values = [(25, 25), (10, 1000), (5, 5), (1, 1)]
    add_coin amt = map ((+ amt) . snd) coin_values
    true_cost low high =
      case lookup (high - low) coin_values of
        Just val -> val
        Nothing -> error $ "invalid costs: " ++ show high ++ ", " ++ show low

dTest obj = dijkstra getAdj getCost (== 10) 0
  where
    getAdj obj = [1, 2, 3, 4, 5]
    getCost a b = if b == 1 then -1 else 10

main :: IO ()
main = do
  print $ countChange 67
  print $ dTest 1

-- print biggestInt
-- print smallest
-- print reallyBig
-- print d2
-- print $ 3 + 2
-- print $ 3 - 2
-- print $ 3 * 2
-- print $ 3 / 2
-- print $ mod 3 299
-- print $ 3 `mod` 2
-- print $ 3 ^ 2
-- print $ (-3) * (-2)
-- print $ div x x
-- print $ True && False
-- print $ not $ False || True
-- print $ mySum 10
-- print $ hailstone 3
-- print $ hailstone 2
-- print $ [2, 3, 4] == 2 : 3 : 4 : []
-- print ex01
-- print ex02
-- print ex03
-- print ex04
-- print k
-- print ex05
-- ambient <- createAmbient (2, 4) 1 1
-- print ambient
-- print $ ambientToMatrix ambient

-- main	| len == 0 = do print "ArgsError: expected n args"
--      	| otherwise = do print "Ok"
--      	where
--       	args = getArgs
--         len <- length args
-- randomValue :: Int -> Int
-- randomValue top = do
-- 	randomIndex <- randomRIO (0, 2)
-- 	return  [1..top] !! randomIndex

-- randomValue :: (Random a, MonadIO m) => m a
-- randomValue = randomRIO (1 :: Int, 5 :: Int)

-- stringToInt a = read a :: Int
-- main = do
-- print 1
-- let a = do
-- 	n <- randomRIO (0, 2)
-- 	return [1..10] !! n
-- print a
-- print $ matrix 4 4 $ \(i,j) -> (indexRandom 4)
-- print "Hola"
-- print m
-- print $ zero 3 4
-- print $ diagonalList 6 2 [1..]
-- print $ permMatrix 3 1 2
-- print $ fromLists [[1,2], [3,4]]
-- print $ getElem 1 2 $ fromLists [[1,2], [3,4]]
-- print $ fromLists [[1,2], [3,4]] !(2,2)
-- print $ setElem 30 (1, 2) $ fromLists [[1,2], [3,4]]
-- print $ even_ 10
-- let e = setElem 30 (1, 2) m
-- let a = 1
-- let m = e
-- print e
-- print m
-- print 1
-- randomIndex <- randomRIO (0, 2)
-- print $ [1,2,3] !! randomIndex

-- run args | (length args) /= 2 = do print "Error"
-- 				 | otherwise = do print "Ok"

-- stringToInt a = read a :: Int
-- main = do
-- 	args <- getArgs
-- 	print $ length args
-- 	run $ map stringToInt args

-- run [x,y] = do
-- 	let m = matrix x y $ \(i,j) -> ( [1..10] !! 1 )
-- 	print m
-- run _ = do print "Error"