{-# LANGUAGE TypeApplications    #-}

module Utils where

--------------------------------------------------------------------------------

cycleSucc :: (Eq a, Enum a, Bounded a) => a -> a
cycleSucc x = if x == maxBound then minBound else succ x

cyclePred :: (Eq a, Enum a, Bounded a) => a -> a
cyclePred x = if x == minBound then maxBound else pred x
