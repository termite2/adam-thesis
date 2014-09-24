{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

state = circle 0.1

stInit     = (text "G"  # fontSize large <> state) # named "stInit"     # moveTo (p2 (0, 0))
stReq      = (text "S2" # fontSize large <> state) # named "stReq"      # moveTo (p2 (1, 0))
stWroteCmd = (text "S4" # fontSize large <> state) # named "stWroteCmd" # moveTo (p2 (1.5, -0.5))
stWroteDat = (text "S3" # fontSize large <> state) # named "stWroteDat" # moveTo (p2 (1.5, 0.5))
stErr      = (text "E"  # fontSize large <> state) # named "stErr"      # moveTo (p2 (2.5, -0.5))
stSending  = (text "S5" # fontSize large <> state) # named "stSending"  # moveTo (p2 (2, 0))
stAck      = (text "S6" # fontSize large <> state) # named "stAck"      # moveTo (p2 (3, 0))

arrowStyleCont  = (with & arrowHead  .~ spike 
                        & headLength .~ large)

arrowStyleUCont = (with & arrowHead  .~ spike 
                        & headLength .~ large
                        & shaftStyle %~ dashingG [0.04, 0.02] 0)

splineShaft = cubicSpline False $ map p2 [(3, 0), (1.5, 0.72), (0, 0)]

arrowSpline = (with & arrowHead  .~ spike 
                    & headLength .~ large
                    & arrowShaft .~ splineShaft)

states = mconcat [stInit, stReq, stWroteCmd, stWroteDat, stErr, stSending, stAck]

labels = mconcat [
        text "send"       # fontSize large # moveTo (p2 (0.5, 0.1)),
        text "write_data" # fontSize large # moveTo (p2 (0.9, 0.3)),
        text "write_cmd"  # fontSize large # moveTo (p2 (0.9, -0.3)),
        text "write_cmd"  # fontSize large # moveTo (p2 (2.1, 0.3)),
        text "write_data" # fontSize large # moveTo (p2 (2.1, -0.3)),
        text "evt_send"   # fontSize large # moveTo (p2 (2.5, 0.1)),
        text "evt_send"   # fontSize large # moveTo (p2 (2, -0.6))
    ]

diag'   = states # connectOutside' arrowStyleUCont "stInit"     "stReq"
                 # connectOutside' arrowStyleCont  "stReq"      "stWroteDat"
                 # connectOutside' arrowStyleCont  "stReq"      "stWroteCmd"
                 # connectOutside' arrowStyleCont  "stWroteDat" "stSending"
                 # connectOutside' arrowStyleCont  "stWroteCmd"  "stSending"
                 # connectOutside' arrowStyleUCont "stSending"   "stAck"
                 # connectOutside' arrowStyleUCont "stWroteCmd"  "stErr"
                 # connectPerim'   arrowSpline     "stAck"       "stInit" (3/8 @@ turn) (1/8 @@ turn)
diag = (diag' <> labels) # pad 1.1

main = mainWith (diag :: Diagram B R2)

--main = mainWith (circle 1 :: Diagram B R2)
