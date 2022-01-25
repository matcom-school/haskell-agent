module Statistics where

median :: [Int] -> Int
median values =
  let total = sum values
      len = length values
   in total `div` len