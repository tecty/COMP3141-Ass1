module TortoiseCombinators
       ( andThen 
       , loop 
       , invisibly 
       , retrace 
       , overlay 
       ) where

import Tortoise

-- See Tests.hs or the assignment spec for specifications for each
-- of these combinators.

andThen :: Instructions -> Instructions -> Instructions
andThen (SetStyle l i1)  i2    = SetStyle  l  $ andThen i1 i2
andThen (PenUp i1)       i2    = PenUp        $ andThen i1 i2
andThen (SetColour c i1) i2    = SetColour c  $ andThen i1 i2
andThen (Move m i1)      i2    = Move      m  $ andThen i1 i2
andThen (PenDown i1)     i2    = PenDown      $ andThen i1 i2
andThen (Turn r i1)      i2    = Turn      r  $ andThen i1 i2
andThen (Stop)           i2    = i2

loop :: Int -> Instructions -> Instructions
loop n i = error "'loop' unimplemented"

invisibly :: Instructions -> Instructions
invisibly i = error "'invisibly' unimplemented"

retrace :: Instructions -> Instructions
retrace i = error "'retrace' unimplemented"

overlay :: [Instructions] -> Instructions
overlay is = error "'overlay' unimplemented"

