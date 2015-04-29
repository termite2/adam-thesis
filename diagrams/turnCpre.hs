{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

fs = local 3

state = circle 4

arrowStyleCont  = (with & arrowHead  .~ spike 
                        & headLength .~ large)

arrowStyleUCont = (with & arrowHead  .~ spike 
                        & headLength .~ large
                        & shaftStyle %~ dashingG [0.8, 0.4] 0)

diag' =  (text "s1" # fontSize fs <> state # named "S1") # moveTo (p2 (0, 0)) 
      <> (text "s2" # fontSize fs <> state # named "S2") # moveTo (p2 (0, 15))
      <> (text "s3" # fontSize fs <> state # named "S3") # moveTo (p2 (0, 35))

      <> state # fc black # named "S4" # moveTo (p2 (20, 0))
      <> state # fc black # named "S5" # moveTo (p2 (20, 25))
      <> state # named "S6" # moveTo (p2 (20, 45))

diag = diag' # connectOutside' arrowStyleCont  "S1" "S4"
             # connectOutside' arrowStyleUCont "S2" "S4"
             # connectOutside' arrowStyleUCont "S2" "S5"
             # connectOutside' arrowStyleUCont "S3" "S5"
             # connectOutside' arrowStyleUCont "S3" "S6"

main = mainWith (diag :: Diagram B)
