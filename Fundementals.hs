{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE TypeApplications    #-}

module Fundementals where

--------------------------------------------------------------------------------

data Accidental = Flat | Sharp
    deriving (Enum)
instance Show Accidental where
    show Flat = "b"   -- ♭
    show Sharp = "'" -- ♯
    

data Chord = MkChord {
    root :: Note, intervals :: Intervals
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


data Note =  -- Represent accidentals by the neighbouring natural tones
    A | AB | B | C | CD | D | DE | E | F | FG | G | GA
    deriving (Eq, Show, Enum, Bounded)

type Notes = [Note]

noteCount :: Int
noteCount = succ $ fromEnum $ maxBound @Note -- | 12

-- Flat aliases
pattern Ab = GA
pattern Bb = AB
pattern Cb = B
pattern Db = CD
pattern Eb = DE
pattern Fb = E
pattern Gb = FG

-- Sharp aliases
pattern A' = AB
pattern B' = C
pattern C' = CD
pattern D' = DE
pattern E' = F
pattern F' = FG
pattern G' = GA


data Interval =
    Uni | Min2 | Maj2 | Min3 | Maj3 | Perf4 | Tri | Perf5 | Min6 | Maj6 | Min7 | Maj7
    deriving (Eq, Ord, Show, Enum, Bounded)

type Intervals = [Interval]

intervalCount :: Int
intervalCount = succ $ fromEnum $ maxBound @Note -- | 12

-- Interval aliases
pattern Root = Uni
pattern Dim4 = Maj3
pattern Aug4 = Tri
pattern Dim5 = Tri
pattern Aug5 = Min6

--------------------------------------------------------------------------------
