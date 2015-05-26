{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

row      = square 40 ||| square 40
row2     = square 40 # fc gray ||| square 40 # fc gray
abstract = row === row2
concrete =  hrule 80 # moveTo (p2 (20, 0)) # dashingN [0.03, 0.03] 0
         <> hrule 80 # moveTo (p2 (20, -40)) # dashingN [0.03, 0.03] 0
         <> vrule 80 # moveTo (p2 (0, -20))  
         <> vrule 80 # moveTo (p2 (40, -20)) 
labels   =  arrowBetween' (with & arrowTail .~ spike') (p2 (-20, 40)) (p2 (60, 40))
         <> arrowBetween' (with & arrowTail .~ spike') (p2 (80, 20)) (p2 (80, -60))
         <> arrowBetween' (with & arrowTail .~ spike') (p2 (20, 25)) (p2 (60, 25))
         <> arrowBetween' (with & arrowTail .~ spike') (p2 (65, 20)) (p2 (65, -20))
varNames =  text "X" # fontSize (local 10) # moveTo (p2 (20, 50))
         <> text "Y" # fontSize (local 10) # moveTo (p2 (90, -20))
         <> text "U" # fontSize (local 6) # moveTo (p2 (40, 30))
         <> text "V" # fontSize (local 6) # moveTo (p2 (70, 0))
subWins  = (rect 20 40 # fc gray # moveTo (p2 (50, 0))) <> (rect 20 40 # fc gray # moveTo (p2 (10, 0)))
diag     = (concrete <> abstract <> labels <> varNames <> subWins) # centerXY # pad 1.1

main = mainWith (diag :: Diagram B)
