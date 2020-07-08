
module Main where

--------------------------------------------------------------------------------

import Fundementals
import Tunings
import Utils
import Numeric.Natural

--------------------------------------------------------------------------------

type Fretting = ([Maybe Natural], Tuning)


data Chord = MkChord Note [Interval]
    deriving (Show)
-- instance Show Chord where
--     show = notate Flat


data Triad =
    Maj |
    Min |
    Dim |
    Aug
instance Show Triad where
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


triadType :: [Interval] -> Maybe Triad
triadType is
    | Maj3 `elem` is && Perf5 `elem` is = Just Maj
    | Min3 `elem` is && Perf5 `elem` is = Just Min
    | Min3 `elem` is && Dim5 `elem` is = Just Dim
    | Maj3 `elem` is && Aug5 `elem` is = Just Aug
    | otherwise = Nothing


noteName :: Accidental -> Note -> String
noteName a n
    | (length $ show n) == 1 = show n
    | otherwise = case a of
        Flat -> (show n !! 1) : show Flat
        Sharp -> (head $ show n) : show Sharp


notate :: Accidental -> Chord -> String
notate a (MkChord r is) = name ++ ext
    where
        name = noteName a r ++ ""
        ext = ""


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
