{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

fs = local 0.08
state = circle 0.1

--OS spec
stOSInit = (text "I"  # fontSize fs <> state) # named "stOSInit" # moveTo (p2 (0, 0))
stOSReq  = (text "S1" # fontSize fs <> state) # named "stOSReq"  # moveTo (p2 (1, 0))
stOSAck  = (text "S2" # fontSize fs <> state) # named "stOSAck"  # moveTo (p2 (2, 0))

--Device spec
stDevOff  = (text "I"  # fontSize fs <> state) # named "stDevOff"  # moveTo (p2 (-1, -1))
stDevOn   = (text "S3" # fontSize fs <> state) # named "stDevOn"   # moveTo (p2 (-1, -2))
stDevBusy = (text "S4" # fontSize fs <> state) # named "stDevBusy" # moveTo (p2 (-1, -3))

--Combined
st11      = (text "I"  # fontSize fs <> state) # named "st11"  # moveTo (p2 (0, -1))
st12      = (text "I"  # fontSize fs <> state) # named "st12"  # moveTo (p2 (1, -1))
st13      = (text "I"  # fontSize fs <> state) # named "st13"  # moveTo (p2 (2, -1))
st21      = (text "I"  # fontSize fs <> state) # named "st21"  # moveTo (p2 (0, -2))
st22      = (text "I"  # fontSize fs <> state) # named "st22"  # moveTo (p2 (1, -2))
st23      = (text "I"  # fontSize fs <> state) # named "st23"  # moveTo (p2 (2, -2))
st31      = (text "I"  # fontSize fs <> state) # named "st31"  # moveTo (p2 (0, -3))
st32      = (text "I"  # fontSize fs <> state) # named "st32"  # moveTo (p2 (1, -3))
st33      = (text "I"  # fontSize fs <> state) # named "st33"  # moveTo (p2 (2, -3))
combined  = [st11, st12, st13, st21, st22, st23, st31, st32, st33]


arrowStyleCont  = (with & arrowHead  .~ spike 
                        & headLength .~ large)

arrowStyleUCont = (with & arrowHead  .~ spike 
                        & headLength .~ large
                        & shaftStyle %~ dashingG [0.04, 0.02] 0)

splineShaft = arc (angleDir (0 @@ turn)) (1/8 @@ turn)

arrowSplineUCont = (with & arrowHead  .~ spike 
                         & headLength .~ large
                         & arrowShaft .~ splineShaft
                         & shaftStyle %~ dashingG [0.04, 0.02] 0)

arrowSplineCont  = (with & arrowHead  .~ spike 
                         & headLength .~ large
                         & arrowShaft .~ splineShaft)

states = mconcat $ [stOSInit, stOSReq, stOSAck, stDevOff, stDevOn, stDevBusy] ++ combined

labels = mconcat [
        text "send_req"          # fontSize fs # moveTo (p2 (0.5, -0.1)),
        text "sent"              # fontSize fs # moveTo (p2 (1.5, -0.1)),
        text "send_ack"          # fontSize fs # moveTo (p2 (1, 0.4)),

        text "ctrl_write(1)"     # fontSize fs # moveTo (p2 (-1.6, -1.5)),
        text "data_write"        # fontSize fs # moveTo (p2 (-1.5, -2.5)),
        text "ctrl_write(0)"     # fontSize fs # moveTo (p2 (-0.4, -1.5)),
        text "sent"              # fontSize fs # moveTo (p2 (-0.6, -2.5))
    ]

diag'   = states # connectOutside' arrowStyleUCont  "stOSInit"     "stOSReq"
                 # connectOutside' arrowStyleCont   "stOSReq"      "stOSAck"
                 # connectPerim'   arrowSplineCont  "stOSAck"      "stOSInit" (3/8 @@ turn) (1/8 @@ turn)

                 # connectPerim'   arrowSplineCont  "stDevOff"  "stDevOn"   (5/8 @@ turn) (3/8 @@ turn)
                 # connectPerim'   arrowSplineCont  "stDevOn"   "stDevOff"  (1/8 @@ turn) (7/8 @@ turn)
                 # connectPerim'   arrowSplineCont  "stDevOn"   "stDevBusy" (5/8 @@ turn) (3/8 @@ turn)
                 # connectPerim'   arrowSplineUCont "stDevBusy" "stDevOn"   (1/8 @@ turn) (7/8 @@ turn)

                 # connectOutside' arrowStyleCont   "st11"      "st12"
                 # connectOutside' arrowSplineCont  "st13"      "st11"
                 # connectOutside' arrowStyleCont   "st21"      "st22"
                 # connectOutside' arrowSplineCont  "st23"      "st21"
                 # connectOutside' arrowStyleCont   "st31"      "st32"
                 # connectOutside' arrowStyleCont   "st32"      "st23"
                 # connectOutside' arrowSplineCont  "st33"      "st31"

                 # connectOutside' arrowSplineCont  "st11"      "st21"
                 # connectOutside' arrowSplineCont  "st21"      "st31"
                 # connectOutside' arrowSplineCont  "st21"      "st11"

                 # connectOutside' arrowSplineCont  "st12"      "st22"
                 # connectOutside' arrowSplineCont  "st22"      "st32"
                 # connectOutside' arrowSplineCont  "st22"      "st12"


initArrow = arrowBetween (p2 (-0.3, 0)) (p2 (-0.1, 0))

diag = (diag' <> labels <> initArrow) # pad 1.5

main = mainWith (diag :: Diagram B)

