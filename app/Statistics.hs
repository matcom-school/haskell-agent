module Statistics where

median :: (Fractional a1, Real a2, Foldable t) => t a2 -> a1
median values =
  let total = sum values
      len = length values
   in realToFrac total / realToFrac len