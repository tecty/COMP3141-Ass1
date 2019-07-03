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
loop n i 
    | n <=  0 || i == Stop = Stop
    | otherwise            = andThen i ( loop (n - 1) i )

    
-- style    = Solid 1
-- colour   = white

invisibly :: Instructions -> Instructions
invisibly i = PenUp (noPenDown True i)
    where 
        noPenDown :: Bool -> Instructions -> Instructions 
        noPenDown b (SetStyle l i1)  = SetStyle  l    $ noPenDown b     i1
        noPenDown b (SetColour c i1) = SetColour c    $ noPenDown b     i1
        noPenDown b (Move m i1)      = Move      m    $ noPenDown b     i1
        noPenDown b (Turn r i1)      = Turn      r    $ noPenDown b     i1
        noPenDown b (PenDown i1)     =                  noPenDown True  i1
        noPenDown b (PenUp i1)       =                  noPenDown False i1
        noPenDown True Stop        =  PenDown Stop
        noPenDown False Stop       =  Stop



retrace :: Instructions -> Instructions
retrace Stop = Stop
retrace i = doRetrace white (Solid 1) True i Stop 
    where
        doRetrace :: Colour -> LineStyle -> Bool -> Instructions -> Instructions -> Instructions 
        doRetrace c l b (SetStyle nl i1)  i2   = doRetrace c  nl b     i1  (SetStyle  l i2)
        doRetrace c l b (SetColour nc i1) i2   = doRetrace nc l  b     i1  (SetColour c i2)
        doRetrace c l b (Move m i1)       i2   = doRetrace c  l  b     i1  (Move      (-m) i2)
        doRetrace c l b (Turn r i1)       i2   = doRetrace c  l  b     i1  (Turn      (360-r) i2)
        doRetrace c l True  (PenUp i1)    i2   = doRetrace c  l  False i1  (PenDown i2)
        doRetrace c l False (PenUp i1)    i2   = doRetrace c  l  False i1  (PenUp   i2)
        doRetrace c l True  (PenDown i1)  i2   = doRetrace c  l  True  i1  (PenDown i2)
        doRetrace c l False (PenDown i1)  i2   = doRetrace c  l  True  i1  (PenUp   i2)
        doRetrace c l True  Stop i = PenDown i
        doRetrace c l False Stop i = PenUp   i

drawAndBackToOrigin :: Instructions -> Instructions 
drawAndBackToOrigin i = andThen i $ (invisibly . retrace) i


overlay :: [Instructions] -> Instructions
overlay is = foldr (\x y-> andThen (drawAndBackToOrigin x) y) Stop is
