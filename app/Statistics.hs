module Statistics where

median :: (Fractional a1, Real a2, Foldable t) => t a2 -> a1
median values =
  let total = sum values
      len = length values
   in realToFrac total / realToFrac len

variance :: (Floating a1, Fractional a2, Real a2) => [a2] -> a1
variance values =
  let xMedian = median values
      xSum = sum $ map (\x -> (x - xMedian) ^ 2) values
      xLen = length values - 1
   in sqrt (realToFrac xSum / realToFrac xLen)
