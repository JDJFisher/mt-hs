{-# LANGUAGE TypeApplications    #-}

module Utils where

--------------------------------------------------------------------------------

csucc :: (Eq a, Enum a, Bounded a) => a -> a
csucc x = if x == maxBound then minBound else succ x


cpred :: (Eq a, Enum a, Bounded a) => a -> a
cpred x = if x == minBound then maxBound else pred x


applyN :: (a -> a) -> Int -> (a -> a)
applyN f n = \x -> iterate f x !! n

--------------------------------------------------------------------------------
