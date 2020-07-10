
module Main where

--------------------------------------------------------------------------------

import Fundementals
import Tunings
import Utils
import Data.Maybe
import Numeric.Natural

--------------------------------------------------------------------------------


type Intervals = [Interval]

data Chord = MkChord {
    root :: Note, intervals :: [Interval]
} deriving (Show)

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
type Fretting = [(Maybe Int, Note)]

--------------------------------------------------------------------------------

flat :: Note -> Note
flat = cpred


sharp :: Note -> Note
sharp = csucc


findIntvl :: Note -> Note -> Interval
findIntvl x y = toEnum $ go x
    where
        go z | z == y = 0
             | otherwise = 1 + go (sharp z)


applyIntvl :: Note -> Interval -> Note
applyIntvl x y = applyN sharp (fromEnum y) x


invert :: Chord -> Chord
invert (MkChord r (i:is)) = MkChord r' is'
    where
        r' = applyIntvl r i
        is' = map (applyN csucc $ fromEnum i) is ++ [findIntvl r' r]


invertN :: Int -> Chord -> Chord
invertN = applyN invert


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
notate a (MkChord r is) = concat [n,q]
    where
        n = noteName a r
        q = fromMaybe "" $ show <$> quality is
        e = "extensions"


describe :: Accidental -> Chord -> String
describe = notate -- | TODO: convert notation to natural language


fretNotes :: Fretting -> Notes
fretNotes ls = [applyIntvl n $ toEnum f | (Just f, n) <- ls]


fromNotes :: Notes -> Chord
fromNotes (l:ls) = MkChord l $ map (findIntvl l) ls
fromNotes _ = error "No notes" -- | TODO: rework


fromFret :: Fretting -> Chord
fromFret = fromNotes . fretNotes

--------------------------------------------------------------------------------

main :: IO()
main = undefined

eM :: Chord
eM = MkChord E [Maj3, Perf5]

c'm :: Chord
c'm = MkChord C' [Min3, Perf5]

og :: Fretting
og = zip [Just 3, Just 2, Just 0, Just 0, Just 0, Just 3] stdt

oa :: Fretting
oa = zip [Nothing, Just 0, Just 2, Just 2, Just 2, Just 0] stdt

stb :: Fretting
stb = zip [Just 7, Just 9, Nothing, Just 8, Nothing, Nothing] stdt
