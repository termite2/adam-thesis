{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

fs = local 0.08

state = circle 0.1

stInit     = (text "I"  # fontSize fs <> state) # named "stInit"     # moveTo (p2 (0, 0))
stReq      = (text "S1" # fontSize fs <> state) # named "stReq"      # moveTo (p2 (1, 0))
stSending  = (text "S2" # fontSize fs <> state) # named "stSending"  # moveTo (p2 (2, 0))
stChecking = (text "S3" # fontSize fs <> state) # named "stChecking" # moveTo (p2 (2, -0.7))
stDone     = (text "G"  # fontSize fs <> state) # named "stDone"     # moveTo (p2 (3, -0.7))

arrowStyleCont  = (with & arrowHead  .~ spike 
                        & headLength .~ large)

arrowStyleUCont = (with & arrowHead  .~ spike 
                        & headLength .~ large
                        & shaftStyle %~ dashingG [0.04, 0.02] 0)

splineShaft = arc (angleDir (0 @@ turn)) (1/8 @@ turn)

arrowSpline = (with & arrowHead  .~ spike 
                    & headLength .~ large
                    & arrowShaft .~ splineShaft
                    & shaftStyle %~ dashingG [0.04, 0.02] 0)

states = mconcat [stInit, stReq, stSending, stChecking, stDone]

labels = mconcat [
        text "send"          # fontSize fs # moveTo (p2 (0.5, 0.1)),
        text "write_data"    # fontSize fs # moveTo (p2 (1.5, 0.1)),
        text "evt_send"      # fontSize fs # moveTo (p2 (2.5, -0.6)),
        text "check"         # fontSize fs # moveTo (p2 (1.65, -0.35)),
        text "busy"          # fontSize fs # moveTo (p2 (2.3, -0.35))
    ]

diag'   = states # connectOutside' arrowStyleUCont "stInit"     "stReq"
                 # connectOutside' arrowStyleCont  "stReq"      "stSending"
                 # connectOutside' arrowStyleUCont "stChecking" "stDone"
                 # connectPerim'   arrowSpline     "stSending"  "stChecking" (5/8 @@ turn) (3/8 @@ turn)
                 # connectPerim'   arrowSpline     "stChecking" "stSending" (1/8 @@ turn) (7/8 @@ turn)

initArrow = arrowBetween (p2 (-0.3, 0)) (p2 (-0.1, 0))

diag = (diag' <> labels <> initArrow) # pad 1.1

main = mainWith (diag :: Diagram B)

