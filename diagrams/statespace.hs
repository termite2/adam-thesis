{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

row      = square 40 ||| (square 40 # fc lightgray)
row2     = square 40 ||| square 40 
abstract = row === row2
concrete =  hrule 80 # moveTo (p2 (20, 0)) # dashingN [0.03, 0.03] 0
         <> hrule 80 # moveTo (p2 (20, -40)) # dashingN [0.03, 0.03] 0
         <> vrule 80 # moveTo (p2 (0, -20))  # dashingN [0.03, 0.03] 0
         <> vrule 80 # moveTo (p2 (40, -20))  # dashingN [0.03, 0.03] 0
labels   =  arrowBetween' (with & arrowTail .~ spike') (p2 (-20, 30)) (p2 (60, 30))
         <> arrowBetween' (with & arrowTail .~ spike') (p2 (70, 20)) (p2 (70, -60))
varNames =  text "X" # fontSize (Local 10) # moveTo (p2 (20, 40))
         <> text "Y" # fontSize (Local 10) # moveTo (p2 (80, -20))
concState = square 20 # fc gray # moveTo (p2 (50, 10)) # lw none
diag     = concState <> concrete <> abstract <> labels <> varNames

main = mainWith (diag :: Diagram B R2)
