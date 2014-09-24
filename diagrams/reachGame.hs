{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

fs = Local 0.08

state = circle 0.1

stInit     = (text "I"  # fontSize fs <> state) # named "stInit"     # moveTo (p2 (0, 0))
stReq      = (text "S2" # fontSize fs <> state) # named "stReq"      # moveTo (p2 (1, 0))
stWroteCmd = (text "S4" # fontSize fs <> state) # named "stWroteCmd" # moveTo (p2 (1.5, -0.5))
stWroteDat = (text "S3" # fontSize fs <> state) # named "stWroteDat" # moveTo (p2 (1.5, 0.5))
stErr      = (text "E"  # fontSize fs <> state) # named "stErr"      # moveTo (p2 (2.5, -0.5))
stSending  = (text "S5" # fontSize fs <> state) # named "stSending"  # moveTo (p2 (2, 0))
stAck      = (text "G"  # fontSize fs <> state) # named "stAck"      # moveTo (p2 (3, 0))

arrowStyleCont  = (with & arrowHead  .~ spike 
                        & headLength .~ large)

arrowStyleUCont = (with & arrowHead  .~ spike 
                        & headLength .~ large
                        & shaftStyle %~ dashingG [0.04, 0.02] 0)

states = mconcat [stInit, stReq, stWroteCmd, stWroteDat, stErr, stSending, stAck]

labels = mconcat [
        text "send"       # fontSize fs # moveTo (p2 (0.5, 0.1)),
        text "write_data" # fontSize fs # moveTo (p2 (0.9, 0.3)),
        text "write_cmd"  # fontSize fs # moveTo (p2 (0.9, -0.3)),
        text "write_cmd"  # fontSize fs # moveTo (p2 (2.1, 0.3)),
        text "write_data" # fontSize fs # moveTo (p2 (2.1, -0.3)),
        text "evt_send"   # fontSize fs # moveTo (p2 (2.5, 0.1)),
        text "evt_send"   # fontSize fs # moveTo (p2 (2, -0.6))
    ]

diag'   = states # connectOutside' arrowStyleUCont "stInit"     "stReq"
                 # connectOutside' arrowStyleCont  "stReq"      "stWroteDat"
                 # connectOutside' arrowStyleCont  "stReq"      "stWroteCmd"
                 # connectOutside' arrowStyleCont  "stWroteDat" "stSending"
                 # connectOutside' arrowStyleCont  "stWroteCmd"  "stSending"
                 # connectOutside' arrowStyleUCont "stSending"   "stAck"
                 # connectOutside' arrowStyleUCont "stWroteCmd"  "stErr"
diag = (diag' <> labels) # pad 1.1

main = mainWith (diag :: Diagram B R2)

