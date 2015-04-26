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
reg = (reg' # centerXY <> text "osState" # fontSize smallF) # moveTo (p2 (30, 0))

comb = (rect 10 20 # named "comb" <> (text "comb logic" # fontSize smallF # rotate (1/4 @@ turn))) # alignBL
i1 = ((text "osSendReq"    # fontSize smallF # moveTo (p2 (0, 2)) <> hrule 15) # alignR <> circle 0.5 # fc black) # moveTo (p2 (0, 5))
i2 = ((text "controllable" # fontSize smallF # moveTo (p2 (0, 2)) <> hrule 15) # alignR  <> circle 0.5 # fc black) # moveTo (p2 (0, 15))
inputs = i1 <> i2 
output = (((text "classSent" # fontSize smallF # moveTo (p2 (-5.5, 0))) <> vrule 10) # alignB <> circle 0.5 # fc black) # moveTo (p2 (3, 20))
combLogic = (comb <> inputs <> output) # (withName "comb" $ \d -> moveOriginTo (location d))

diag = combLogic <> reg <> hrule 20 # alignL # moveTo (p2 (5, 3.5)) <> circle 0.5 # fc black # moveTo (p2 (25, 3.5)) <> fromVertices [p2 (35, 3.5), p2 (45, 3.5), p2 (45, 13.5), p2 (2, 13.5), p2 (2, 10)] <> circle 0.5 # fc black # moveTo (p2 (2, 10))

main = mainWith (diag :: Diagram B)

