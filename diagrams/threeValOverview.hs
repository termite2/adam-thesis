{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

fs = local 2.5

diag =  (text "T" # fontSize fs <> (circle 5 # fc darkgray)) 
     <> (text "CPre<span rise='-1800'>1</span><span rise='1800'>M</span>(W<span rise='1800'>M</span>)" # fontSize fs # moveTo (p2 (-30, 5)))
     <> (text "CPre<span rise='-1800'>1</span><span rise='1800'>m</span>(W<span rise='1800'>M</span>)" # fontSize fs # moveTo (p2 (-95, 5)))
     <> (text "CPre<span rise='-1800'>1</span><span rise='1800'>m</span>(W<span rise='1800'>m</span>)" # fontSize fs # moveTo (p2 (-140, 5)))
     <> arrowBetween (p2(-70, 0)) (p2 (-85, 0))
     <> arrowBetween (p2(-120, 0)) (p2 (-105, 0))
     <> (text "W<span rise='-1800'>M</span>=Cpre<span rise='-1800'>1</span><span rise='1800'>M</span>(W<span rise='1800'>M</span>) ∪ T" # fontSize fs # moveTo (p2 (-32.5, -25)))
     <> arrowBetween' (with & arrowHead .~ spike & arrowTail .~ spike') (p2 (-70, -30)) (p2 (5, -30))
     <> vrule 35 # dashingN [0.01, 0.01] 0 # alignT # moveTo (p2 (5, 0))
     <> vrule 35 # dashingN [0.01, 0.01] 0 # alignT # moveTo (p2 (-70, 0))
     <> (circle 10  # scaleX 3.5 # alignR # fc gray) 
     <> (circle 15 # scaleX 4 # alignR # fc lightgray # dashingN [0.03, 0.03] 0) 
     <> (circle 20 # scaleX 4 # alignR) 

main = mainWith (diag :: Diagram B)
