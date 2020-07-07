
module Tunings where

--------------------------------------------------------------------------------

import Fundementals

--------------------------------------------------------------------------------

type Tuning = (String, [Note])


stdt :: Tuning
stdt = ("Standard", [E, A, D, G, B, E])

hsdt :: Tuning
hsdt = ("Half-step down", [Eb, Ab, Db, Gb, Bb, Eb])

fsdt :: Tuning
fsdt = ("Full-step down", [D, G, C, F, A, D])

ddt :: Tuning
ddt = ("Drop D", [D, A, D, G, B, E])
