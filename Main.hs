module Main where

--------------------------------------------------------------------------------

import Fundementals
import Tunings
import Utils
import Maybe
import Numeric.Natural

--------------------------------------------------------------------------------

type Fretting = [Maybe Natural]
type Voicing = (Fretting, Tuning)

type Intervals = [Natural]
type Chord = (Note, Intervals)

--------------------------------------------------------------------------------

sharp :: Note -> Note
sharp = cycleSucc


flat :: Note -> Note
flat = cyclePred


toChord :: Voicing -> Chord
toChord (f, t) =


-- invert :: Chord -> Chord
-- invert = undefined


-- notate :: Chord -> String
-- notate = undefined


-- describe :: Chord -> String
-- describe = undefined

--------------------------------------------------------------------------------

main :: IO()
main = undefined


og :: Voicing
og = ([Just 3, Just 2, Just 0, Just 0, Just 0, Just 3], stdt)


oa :: Voicing
oa = ([Nothing, Just 0, Just 2, Just 2, Just 2, Just 0], stdt)
