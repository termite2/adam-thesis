{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

row      = square 40 ||| (square 40 # fc lightgray)
row2     = square 40 ||| square 40 
abstract = row === row2
concrete =  hrule 80 # moveTo (p2 (20, 0)) # dashingN [0.03, 0.03] 0
         <> hrule 80 # moveTo (p2 (20, -40)) # dashingN [0.03, 0.03] 0
         <> vrule 80 # moveTo (p2 (0, -20))  # dashingN [0.03, 0.03] 0
         <> vrule 80 # moveTo (p2 (40, -20))  # dashingN [0.03, 0.03] 0
labels   =  arrowBetween' (with & arrowTail .~ spike') (p2 (-20, 40)) (p2 (60, 40))
         <> arrowBetween' (with & arrowTail .~ spike') (p2 (80, 20)) (p2 (80, -60))
         <> arrowBetween' (with & arrowTail .~ spike') (p2 (20, 25)) (p2 (60, 25))
         <> arrowBetween' (with & arrowTail .~ spike') (p2 (65, 20)) (p2 (65, -20))
varNames =  text "X" # fontSize (local 10) # moveTo (p2 (20, 50))
         <> text "Y" # fontSize (local 10) # moveTo (p2 (90, -20))
         <> text "U" # fontSize (local 6) # moveTo (p2 (40, 30))
         <> text "V" # fontSize (local 6) # moveTo (p2 (70, 0))
stateLab =  text "x" # fontSize (local 6) # moveTo (p2 (50, 10))
concState = square 20 # fc gray # moveTo (p2 (50, 10)) # lw none
diag     = (stateLab <> concState <> concrete <> abstract <> labels <> varNames) # centerXY # pad 1.1

main = mainWith (diag :: Diagram B)
