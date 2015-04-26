{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

import Diagrams.Prelude hiding (output)
import Diagrams.Backend.Cairo.CmdLine

fs = local 2
smallF = local 1.5

reg' =  triangle 1.5 # rotate (1/12 @@ turn) 
                     # alignL 
                     # moveTo (p2 (0, 3)) 
     <> text "D" # fontSize fs # moveTo (p2 (1.8, 11))
     <> text "Q" # fontSize fs # moveTo (p2 (8, 11))
     <> rect 10 15   # alignBL 
reg = (reg' # centerXY <> text "devState" # fontSize smallF) # moveTo (p2 (30, 0))

comb = (rect 10 20 # named "comb" <> (text "comb logic" # fontSize smallF # rotate (1/4 @@ turn))) # alignBL
i1 = ((text "controllable" # fontSize smallF # moveTo (p2 (0, 2)) <> hrule 15) # alignR <> circle 0.5 # fc black) # moveTo (p2 (0, 4))
i2 = ((text "devSent"      # fontSize smallF # moveTo (p2 (0, 2)) <> hrule 15) # alignR  <> circle 0.5 # fc black) # moveTo (p2 (0, 10))
i3 = ((text "devSendReq"   # fontSize smallF # moveTo (p2 (0, 2)) <> hrule 15) # alignR  <> circle 0.5 # fc black) # moveTo (p2 (0, 16))
inputs = i1 <> i2 <> i3
output = ((text "classSent" # fontSize smallF # moveTo (p2 (5, 0))) <> vrule 10) # alignT # moveTo (p2 (5, 0))
combLogic = (comb <> inputs <> output) # (withName "comb" $ \d -> moveOriginTo (location d))

diag = combLogic <> reg <> hrule 20 # alignL # moveTo (p2 (5, 3.5)) <> circle 0.5 # fc black # moveTo (p2 (25, 3.5)) <> fromVertices [p2 (35, 3.5), p2 (45, 3.5), p2 (45, 13.5), p2 (0, 13.5), p2 (0, 10)] <> circle 0.5 # fc black # moveTo (p2 (0, 10))

main = mainWith (diag :: Diagram B)

