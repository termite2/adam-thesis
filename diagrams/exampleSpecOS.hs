{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

fs = Local 0.08

state = circle 0.1

osIdle      = (text "O0" # fontSize fs <> state) # named "osIdle"      # moveTo (p2 (0, 1))
osRequested = (text "O1" # fontSize fs <> state) # named "osRequested" # moveTo (p2 (1, 1))
osDone      = (text "O2" # fontSize fs <> state) # named "osDone"      # moveTo (p2 (2, 1))
osError     = (text "O3" # fontSize fs <> state) # named "osError"     # moveTo (p2 (1, 0))

arrowStyleCont  = (with & arrowHead  .~ spike 
                        & headLength .~ large)

arrowStyleUCont = (with & arrowHead  .~ spike 
                        & headLength .~ large
                        & shaftStyle %~ dashingG [0.04, 0.02] 0)

splineShaft = arcCW (1/8 @@ turn) (0 @@ turn)

arrowSplineCont = (with & arrowHead  .~ spike 
                    & headLength .~ large
                    & arrowShaft .~ splineShaft)

arrowSplineUCont = (with & arrowHead  .~ spike 
                    & headLength .~ large
                    & arrowShaft .~ splineShaft
                    & shaftStyle %~ dashingG [0.04, 0.02] 0)

states = mconcat [osIdle, osRequested, osDone, osError]

labels = mconcat [
        text "devSendReq" # fontSize fs # moveTo (p2 (0.5, 1.2)),
        text "classSent"  # fontSize fs # moveTo (p2 (1.5, 1.2)),
        text "classSent"  # fontSize fs # moveTo (p2 (0.15, 0.45))
    ]

diag'   = states # connectOutside' arrowStyleCont  "osIdle"      "osRequested" 
                 # connectOutside' arrowStyleUCont "osIdle"      "osError"    
                 # connectOutside' arrowStyleUCont "osRequested" "osDone"    

initArrow = arrowBetween (p2 (-0.3, 1)) (p2 (-0.1, 1)) 

diag = (diag' <> labels <> initArrow) # pad 1.6

main = mainWith (diag :: Diagram B R2)
