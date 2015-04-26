{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, GADTs #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

fs = local 0.08

state = circle 0.1

osIdleDevIdle         = (text "00" # fontSize fs <> state) # named "osIdle_devIdle"         # moveTo (p2 (0, 1))
osRequestedDevIdle    = (text "01" # fontSize fs <> state) # named "osRequested_devIdle"    # moveTo (p2 (1, 1))
osDoneDevIdle         = (text "02" # fontSize fs <> state) # named "osDone_devIdle"         # moveTo (p2 (2, 1))
osErrorDevIdle        = (text "03" # fontSize fs <> state) # named "osError_devIdle"        # moveTo (p2 (1, 0))
osIdleDevSending      = (text "10" # fontSize fs <> state) # named "osIdle_devSending"      # moveTo (p2 (0, 3))
osRequestedDevSending = (text "11" # fontSize fs <> state) # named "osRequested_devSending" # moveTo (p2 (1, 3))
osDoneDevSending      = (text "12" # fontSize fs <> state) # named "osDone_devSending"      # moveTo (p2 (2, 3))
osErrorDevSending     = (text "13" # fontSize fs <> state) # named "osError_devSending"     # moveTo (p2 (1, 2))

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
        --text "devSendReq" # fontSize fs # moveTo (p2 (0.5, 1.2)),
        --text "classSent"  # fontSize fs # moveTo (p2 (1.5, 1.2)),
        --text "classSent"  # fontSize fs # moveTo (p2 (0.15, 0.45))
    ]

diag'   = states # connectOutside' arrowStyleCont  "osIdle_devIdle"         "osRequested_devIdle" 
                 # connectOutside' arrowStyleCont  "osIdle_devSending"      "osRequested_devSending" 

                 # connectOutside' arrowStyleUCont "osIdle_devIdle"         "osError_devIdle"    
                 # connectOutside' arrowStyleUCont "osIdle_devSending"      "osError_devSending"    

                 # connectOutside' arrowStyleUCont "osRequested_devIdle"    "osDone_devIdle"    
                 # connectOutside' arrowStyleUCont "osRequested_devSending" "osDone_devSending"    

                 # connectPerim' arrowSplineCont  "osIdle_devIdle"          "osIdle_devSending" (3/8 @@ turn) (5/8 @@ turn)
                 # connectPerim' arrowSplineCont  "osRequested_devIdle"     "osRequested_devSending" (3/8 @@ turn) (5/8 @@ turn)
                 # connectPerim' arrowSplineCont  "osDone_devIdle"          "osDone_devSending" (3/8 @@ turn) (5/8 @@ turn)
                 # connectPerim' arrowSplineCont  "osError_devIdle"         "osError_devSending" (3/8 @@ turn) (5/8 @@ turn)

                 -- # connectPerim' arrowSplineUCont "devSending" "devIdle"    (7/8 @@ turn) (1/8 @@ turn)

initArrow = arrowBetween (p2 (-0.3, 1)) (p2 (-0.1, 1)) 

diag = (diag' <> labels <> initArrow) # pad 1.6

main = mainWith (diag :: Diagram B)

