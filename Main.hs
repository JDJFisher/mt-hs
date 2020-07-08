
module Main where

--------------------------------------------------------------------------------

import Fundementals
import Tunings
import Utils
import Data.Maybe
import Numeric.Natural

--------------------------------------------------------------------------------

type Fretting = ([Maybe Natural], Tuning)

type Intervals = [Interval]

data Chord = MkChord Note [Interval]
    deriving (Show)
-- instance Show Chord where
--     show = notate Flat


data Quality =
    Maj |
    Min |
    Dim |
    Aug
    deriving (Eq)
instance Show Quality where
    show Maj = ""
    show Min = "m"
    show Dim = "dim"
    show Aug = "aug"

--------------------------------------------------------------------------------

flat :: Note -> Note
flat = cyclePred


sharp :: Note -> Note
sharp = cycleSucc


interval :: Note -> Note -> Interval
interval x y = toEnum $ (fromEnum y - fromEnum x + noteCount) `mod` noteCount


note :: Note -> Interval -> Note
note x y = toEnum $ (fromEnum x + fromEnum y) `mod` intervalCount


invert :: Chord -> Chord
invert (MkChord r (i:is)) = MkChord r' is'
    where
        r' = note r i
        is' = map (\x -> interval r' $ note r x) is ++ [interval r' r]


invertN :: Int -> Chord -> Chord
invertN n c = iterate invert c !! n


isSeventh :: Intervals -> Bool
isSeventh is = hasThird is && any (`elem` is) [Min7, Maj7]


isTriad :: Intervals -> Bool
isTriad is = hasThird is && hasFifth is


hasThird :: Intervals -> Bool
hasThird is = any (`elem` is) [Min3, Maj3]


hasFifth :: Intervals -> Bool
hasFifth is = any (`elem` is) [Dim5, Perf5, Aug5]


quality :: Intervals -> Maybe Quality
quality is
    | Min3 `elem` is && Dim5 `elem` is = Just Dim
    | Maj3 `elem` is && Aug5 `elem` is = Just Aug
    | Maj3 `elem` is = Just Maj
    | Min3 `elem` is = Just Min
    | otherwise = Nothing


noteName :: Accidental -> Note -> String
noteName a n
    | (length $ show n) == 1 = show n
    | otherwise = case a of
        Flat -> (show n !! 1) : show Flat
        Sharp -> (head $ show n) : show Sharp


notate :: Accidental -> Chord -> String
notate a (MkChord r is) = n ++ q ++ e
    where
        n = noteName a r
        q = show $ quality is
        e = ""


describe :: Accidental -> Chord -> String
describe = notate

--------------------------------------------------------------------------------

main :: IO()
main = undefined

eM :: Chord
eM = MkChord E [Maj3, Perf5]

c'm :: Chord
c'm = MkChord C' [Min3, Perf5]

og :: Fretting
og = ([Just 3, Just 2, Just 0, Just 0, Just 0, Just 3], stdt)

oa :: Fretting
oa = ([Nothing, Just 0, Just 2, Just 2, Just 2, Just 0], stdt)
