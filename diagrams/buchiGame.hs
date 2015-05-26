{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

fs = local 0.08

state = circle 0.1

stInit     = (text "G"  # fontSize fs <> state) # named "stInit"     # moveTo (p2 (0, 0))
stReq      = (text "S1" # fontSize fs <> state) # named "stReq"      # moveTo (p2 (1, 0))
stSending  = (text "S2" # fontSize fs <> state) # named "stSending"  # moveTo (p2 (2, 0))
stAck      = (text "S3" # fontSize fs <> circle 0.08 <> state) # named "stAck"      # moveTo (p2 (3, 0))
stWrong    = (text "S4" # fontSize fs <> state) # named "stWrong"    # moveTo (p2 (2, -0.5))
stDead     = (text "S5" # fontSize fs <> circle 0.08 <> state) # named "stDead"     # moveTo (p2 (3, -0.5))

arrowStyleCont  = (with & arrowHead  .~ spike 
                        & headLength .~ large)

arrowStyleUCont = (with & arrowHead  .~ spike 
                        & headLength .~ large
                        & shaftStyle %~ dashingG [0.04, 0.02] 0)

splineShaft = cubicSpline False $ map p2 [(3, 0), (1.5, 0.4), (0, 0)]

arrowSpline = (with & arrowHead  .~ spike 
                    & headLength .~ large
                    & arrowShaft .~ splineShaft)

states = mconcat [stInit, stReq, stSending, stAck, stWrong, stDead]

labels = mconcat [
        text "send"          # fontSize fs # moveTo (p2 (0.5, 0.1)),
        text "write_data"    # fontSize fs # moveTo (p2 (1.5, 0.1)),
        text "evt_send"      # fontSize fs # moveTo (p2 (2.5, 0.1)),
        text "send_ack"      # fontSize fs # moveTo (p2 (1.5, 0.53)),
        text "evt_send"      # fontSize fs # moveTo (p2 (2.5, -0.4)),
        text "self_destruct" # fontSize fs # moveTo (p2 (1.2, -0.32))
    ]

diag'   = states # connectOutside' arrowStyleUCont "stInit"    "stReq"
                 # connectOutside' arrowStyleCont  "stReq"     "stSending"
                 # connectOutside' arrowStyleUCont "stSending" "stAck"
                 # connectOutside' arrowStyleCont  "stReq"     "stWrong"
                 # connectOutside' arrowStyleUCont "stWrong"   "stDead"
                 # connectPerim'   arrowSpline     "stAck"     "stInit" (3/8 @@ turn) (1/8 @@ turn)

initArrow = arrowBetween (p2 (-0.3, 0)) (p2 (-0.1, 0))

diag = (diag' <> labels <> initArrow) # pad 1.1

main = mainWith (diag :: Diagram B)

