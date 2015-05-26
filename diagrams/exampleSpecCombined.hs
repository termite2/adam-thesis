{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

fs = local 0.08

state = circle 0.1

osIdleDevIdle         = (text "00" # fontSize fs <> state) # named "osIdle_devIdle"         # moveTo (p2 (0, 1))
osRequestedDevIdle    = (text "01" # fontSize fs <> state) # named "osRequested_devIdle"    # moveTo (p2 (1, 1))
osDoneDevIdle         = (text "02" # fontSize fs <> circle 0.08 <> state) # named "osDone_devIdle"         # moveTo (p2 (2, 1))
osErrorDevIdle        = (text "03" # fontSize fs <> state) # named "osError_devIdle"        # moveTo (p2 (-1, 1))
osIdleDevSending      = (text "10" # fontSize fs <> state) # named "osIdle_devSending"      # moveTo (p2 (0, 3))
osRequestedDevSending = (text "11" # fontSize fs <> state) # named "osRequested_devSending" # moveTo (p2 (1, 3))
osDoneDevSending      = (text "12" # fontSize fs <> circle 0.08 <> state) # named "osDone_devSending"      # moveTo (p2 (2, 3))
osErrorDevSending     = (text "13" # fontSize fs <> state) # named "osError_devSending"     # moveTo (p2 (-1, 3))

arrowStyleCont  = (with & arrowHead  .~ spike 
                        & headLength .~ large)

arrowStyleUCont = (with & arrowHead  .~ spike 
                        & headLength .~ large
                        & shaftStyle %~ dashingG [0.04, 0.02] 0)

splineShaft = arcCW (angleDir (1/8 @@ turn)) (angleDir (0 @@ turn))

arrowSplineCont = (with & arrowHead  .~ spike 
                    & headLength .~ large
                    & arrowShaft .~ splineShaft)

arrowSplineUCont = (with & arrowHead  .~ spike 
                    & headLength .~ large
                    & arrowShaft .~ splineShaft
                    & shaftStyle %~ dashingG [0.04, 0.02] 0)

states = mconcat [osIdleDevIdle, osRequestedDevIdle, osDoneDevIdle, osErrorDevIdle, osIdleDevSending, osRequestedDevSending, osDoneDevSending, osErrorDevSending]

labels = mconcat [
        text "osSendReq" # fontSize fs # moveTo (p2 (0.5, 3.2)),
        text "osSendReq" # fontSize fs # moveTo (p2 (0.5, 0.8)),

        text "classSent" # fontSize fs # rotate (1 - (1.107 / (2*pi)) @@ turn) # moveTo (p2 (1.5, 1.7)),
        text "classSent" # fontSize fs # rotate (1.107 / (2*pi) @@ turn) # moveTo (p2 (-0.5, 1.7)),

        text "devSendReq" # fontSize fs # rotate (0.25 @@ turn) # moveTo (p2 (0.1, 2)),
        text "devSendReq" # fontSize fs # rotate (0.25 @@ turn) # moveTo (p2 (0.9, 2)),

        text "classSent" # fontSize fs # rotate (0.25 @@ turn) # moveTo (p2 (2.4, 2)),
        text "classSent" # fontSize fs # rotate (0.25 @@ turn) # moveTo (p2 (-0.9, 2)),

        text "devSendReq" # fontSize fs # rotate (0.25 @@ turn) # moveTo (p2 (-1.4, 2)),
        text "devSendReq" # fontSize fs # rotate (0.25 @@ turn) # moveTo (p2 (1.9, 2))
    ]

diag'   = states # connectOutside' arrowStyleUCont  "osIdle_devIdle"         "osRequested_devIdle" 
                 # connectOutside' arrowStyleUCont  "osIdle_devSending"      "osRequested_devSending" 


                 # connectOutside' arrowStyleCont  "osIdle_devIdle"          "osIdle_devSending" 
                 # connectOutside' arrowStyleCont  "osRequested_devIdle"     "osRequested_devSending" 
                 # connectOutside' arrowStyleCont  "osDone_devIdle"          "osDone_devSending" 
                 # connectPerim' arrowSplineCont  "osError_devIdle"         "osError_devSending" (3/8 @@ turn) (5/8 @@ turn)

                 # connectOutside' arrowStyleUCont "osRequested_devSending" "osDone_devIdle" 
                 # connectPerim' arrowSplineUCont "osDone_devSending"      "osDone_devIdle"           (7/8 @@ turn) (1/8 @@ turn)
                 # connectOutside' arrowStyleUCont "osError_devSending"     "osError_devIdle"
                 # connectOutside' arrowStyleUCont "osIdle_devSending"     "osError_devIdle"          


initArrow = arrowBetween (p2 (0, 0.6)) (p2 (0, 0.9)) 

diag = (diag' <> labels <> initArrow) # centerXY # pad 1.1

main = mainWith (diag :: Diagram B)

