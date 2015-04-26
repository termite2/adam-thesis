{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

fs = local 2

root = (text "x" # fontSize fs <> circle 5) # named "root"
n1   = (text "y" # fontSize fs <> circle 5) # named "n1" # moveTo (p2 (-20, -20))
n2   = (text "y" # fontSize fs <> circle 5) # named "n2" # moveTo (p2 (20, -20)) 
n3   = (text "z" # fontSize fs <> circle 5) # named "n3" # moveTo (p2 (20, -40)) 
n4   = (text "z" # fontSize fs <> circle 5) # named "n4" # moveTo (p2 (40, -40)) 

t1   = (text "0" # fontSize fs <> roundedRect 10 10 1) # named "t1" # moveTo (p2 (-10, -60))     
t2   = (text "1" # fontSize fs <> roundedRect 10 10 1) # named "t2" # moveTo (p2 (30, -60))     

elseStyle = with & shaftStyle %~ dashingG [1, 0.5] 0

states = root <> n1 <> n2 <> n3 <> n4 <> t1 <> t2
diag = states # connectOutside' elseStyle "root" "n1"
              # connectOutside "root" "n2"
              # connectOutside' elseStyle "n2" "n3"
              # connectOutside "n2" "n4"
              # connectOutside "n3" "t1"
              # connectOutside' elseStyle "n3" "t2"
              # connectOutside' elseStyle "n4" "t1"
              # connectOutside "n4" "t2"
              # connectOutside "n1" "t1"
              # connectOutside' elseStyle "n1" "t2"

main = mainWith (diag :: Diagram B)

