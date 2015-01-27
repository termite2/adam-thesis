{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

fs = Local 0.08

state = circle 0.1

devIdle    = (text "D0" # fontSize fs <> state) # named "devIdle"    # moveTo (p2 (0, 0))
devSending = (text "D1" # fontSize fs <> state) # named "devSending" # moveTo (p2 (0, 1))

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

states = mconcat [devIdle, devSending]

labels = mconcat [
        text "devSendReq" # fontSize fs # moveTo (p2 (0.55, 0.5)),
        text "classSent"  # fontSize fs # moveTo (p2 (-0.55, 0.5))
    ]

diag'   = states # connectPerim' arrowSplineCont  "devIdle"    "devSending" (3/8 @@ turn) (5/8 @@ turn)
                 # connectPerim' arrowSplineUCont "devSending" "devIdle"    (7/8 @@ turn) (1/8 @@ turn)

initArrow = arrowBetween (p2 (-0.3, 0)) (p2 (-0.1, 0)) 

diag = (diag' <> labels <> initArrow) # pad 1.6

main = mainWith (diag :: Diagram B R2)

