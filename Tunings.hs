
module Tunings where

--------------------------------------------------------------------------------

import Fundementals
import Data.Map

--------------------------------------------------------------------------------

type Tuning = [Note]

--------------------------------------------------------------------------------

tunings :: Map String Tuning
tunings = fromList
    [ ("Standard", stdt)
    , ("Half-step down", hsdt)
    , ("Full-step down", fsdt)
    , ("Drop D", ddt) ]

stdt :: Tuning
stdt = [E, A, D, G, B, E]

hsdt :: Tuning
hsdt = [Eb, Ab, Db, Gb, Bb, Eb]

fsdt :: Tuning
fsdt = [D, G, C, F, A, D]

ddt :: Tuning
ddt = [D, A, D, G, B, E]

--------------------------------------------------------------------------------
