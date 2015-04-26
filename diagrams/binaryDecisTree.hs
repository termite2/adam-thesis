{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

fs = local 2

root = (text "x" # fontSize fs <> circle 5) # named "root"
n1   = (text "y" # fontSize fs <> circle 5) # named "n1" # moveTo (p2 (-20, -20))
n2   = (text "y" # fontSize fs <> circle 5) # named "n2" # moveTo (p2 (20, -20)) 

t1   = (text "0" # fontSize fs <> roundedRect 10 10 1) # named "t1" # moveTo (p2 (-30, -40))    
t2   = (text "1" # fontSize fs <> roundedRect 10 10 1) # named "t2" # moveTo (p2 (-10, -40))    
t3   = (text "1" # fontSize fs <> roundedRect 10 10 1) # named "t3" # moveTo (p2 (10, -40))     
t4   = (text "1" # fontSize fs <> roundedRect 10 10 1) # named "t4" # moveTo (p2 (30, -40))     

elseStyle = with & shaftStyle %~ dashingG [1, 0.5] 0

states = root <> n1 <> n2 <> t1 <> t2 <> t3 <> t4
diag = states # connectOutside' elseStyle "root" "n1"
              # connectOutside "root" "n2"
              # connectOutside' elseStyle "n1" "t1"
              # connectOutside "n1" "t2"
              # connectOutside' elseStyle "n2" "t3"
              # connectOutside "n2" "t4"

main = mainWith (diag :: Diagram B)

