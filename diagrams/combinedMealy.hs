
{-# LANGUAGE NoMonomorphismRestriction #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

fs = Local 2
smallF = Local 1.5

reg' =  triangle 1.5 # rotate (1/12 @@ turn) 
                     # alignL 
                     # moveTo (p2 (0, 3)) 
     <> text "D" # fontSize fs # moveTo (p2 (1.8, 11))
     <> text "Q" # fontSize fs # moveTo (p2 (8, 11))
     <> rect 10 15   # alignBL 
reg = (reg' # centerXY <> text "osState" # fontSize smallF) # moveTo (p2 (30, 0))

comb = (rect 10 20 # named "comb" <> (text "comb logic" # fontSize smallF # rotate (1/4 @@ turn))) # alignBL
i1 = ((text "osSendReq"    # fontSize smallF # moveTo (p2 (0, 2)) <> hrule 15) # alignR <> circle 0.5 # fc black) # moveTo (p2 (0, 5))
i2 = (hrule 5 # alignR <> circle 0.5 # fc black) # moveTo (p2 (0, 15))
inputs = i1 <> i2 
output = (vrule 10 # alignB <> circle 0.5 # fc black) # moveTo (p2 (3, 20))
combLogic = (comb <> inputs <> output) # (withName "comb" $ \d -> moveOriginTo (location d))

diag = combLogic <> reg <> hrule 20 # alignL # moveTo (p2 (5, 3.5)) <> circle 0.5 # fc black # moveTo (p2 (25, 3.5)) <> fromVertices [p2 (35, 3.5), p2 (45, 3.5), p2 (45, 13.5), p2 (2, 13.5), p2 (2, 10)] <> circle 0.5 # fc black # moveTo (p2 (2, 10))

regd' =  triangle 1.5 # rotate (1/12 @@ turn) 
                     # alignL 
                     # moveTo (p2 (0, 3)) 
     <> text "D" # fontSize fs # moveTo (p2 (1.8, 11))
     <> text "Q" # fontSize fs # moveTo (p2 (8, 11))
     <> rect 10 15   # alignBL 
regd = (regd' # centerXY <> text "devState" # fontSize smallF) # moveTo (p2 (30, 0))

combd = (rect 10 20 # named "comb" <> (text "comb logic" # fontSize smallF # rotate (1/4 @@ turn))) # alignBL
i1d = ((text "controllable" # fontSize smallF # moveTo (p2 (0, 2)) <> hrule 15) # alignR <> circle 0.5 # fc black) # moveTo (p2 (0, 4))
i2d = ((text "devSent"      # fontSize smallF # moveTo (p2 (0, 2)) <> hrule 15) # alignR  <> circle 0.5 # fc black) # moveTo (p2 (0, 10))
i3d = ((text "devSendReq"   # fontSize smallF # moveTo (p2 (0, 2)) <> hrule 15) # alignR  <> circle 0.5 # fc black) # moveTo (p2 (0, 16))
inputsd = i1d <> i2d <> i3d
outputd = vrule 5 # alignT # moveTo (p2 (3, 0))
combLogicd = (combd <> inputsd <> outputd) # (withName "comb" $ \d -> moveOriginTo (location d))

diagd = combLogicd <> regd <> hrule 20 # alignL # moveTo (p2 (5, 3.5)) <> circle 0.5 # fc black # moveTo (p2 (25, 3.5)) <> fromVertices [p2 (35, 3.5), p2 (45, 3.5), p2 (45, 13.5), p2 (0, 13.5), p2 (0, 10)] <> circle 0.5 # fc black # moveTo (p2 (0, 10))

combined = (diagd === diag) <> text "classSent" # fontSize smallF # moveTo (p2 (3, -17)) <> vrule 24 # alignB # moveTo (p2 (-10, -30))

main = mainWith (combined :: Diagram B R2)

