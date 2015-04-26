{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

diag = (text "T" <> (circle 5 # fc darkgray)) <> (circle 12 # scaleX 3.5 # alignR # fc gray) <> (circle 15 # scaleX 4 # alignR # fc lightgray # dashingN [0.03, 0.03] 0) <> (circle 20 # scaleX 4 # alignR) 

main = mainWith (diag :: Diagram B)
