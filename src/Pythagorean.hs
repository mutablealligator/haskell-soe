module Pythagorean where

import SOE

type Line = ((Double, Double), (Double, Double))

-- scale the length of the line by the given factor;
-- point of origin is the same as the input line
scaleLine :: Double -> Line -> Line
scaleLine f ((x1, y1), (x2, y2))
  = ((x1, y1), (x1 + (x2 -x1) * f , y1 + (y2 - y1) * f))

-- rotate line by given angle about the origin
-- (pi/4 is 90 degree clockwise, pi/2 180, and so on) 
rotateLine :: Double -> Line -> Line
rotateLine alpha l@(p1@(x1, y1), p2@(x2, y2)) 
  = ((x1, y1), ((cos   alpha) * nx - (sin  alpha) * ny + x1,
     (sin  alpha) * nx +(cos  alpha) * ny + y1))
  where
    (nx, ny) = (x2 - x1, y2 - y1)


-- constructs the tree as one single path
fractalTree :: Window -> Double -> Double -> Line -> IO()
fractalTree w factor n line = fractalTree' w n line
  where
    fractalTree' :: Window -> Double -> Line -> IO()
    fractalTree' w 0 line = []  
    fractalTree' w n line 
      = [p1, p4] ++ fractalTree' w (n-1) (p4,p5) ++
                    fractalTree' w (n-1) (p5,p3) ++
        [p3,p2] 
      where 
        flipLine (pS, pE) = (pE, pS)
        [p1,p2,p3,p4,_]   = drawInWindow w ( withColor Blue (polygon [p1,p2,p3,p4]))
        (_, p5)           = rotateLine (factor * pi) $ 
                              flipLine $ 
                                scaleLine 0.5 $ (p3, p4)

-- Drae the Sierpinski Carpet on a Window
pTree :: IO ()
pTree =  runGraphics(
                      do w <- openWindow "Pythagoream Fractal Tree" (1000,1000)
                         pTree w 0.5 13 ((470.0,800.0), (360.0,800.0))
                         k <- getKey w
                         closeWindow w
                    )
