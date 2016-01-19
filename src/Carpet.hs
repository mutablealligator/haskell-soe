module Carpet where
import SOE
import Play

-- Define Minimum Size
minimumSize :: Int
minimumSize = 5

-- Draw a Rectangle with Blue Color and specified co-ordinates
fillRectangle            :: Window -> Int -> Int -> Int -> IO()
fillRectangle w x y size = drawInWindow w ( withColor Blue 
                             (polygon [(x+1, y+1), -- Bottom Left
                                       (x+3, y+3), -- Top Right
                                       (x+1, y+3), -- Top Left
                                       (x+3, y+1)  -- Bottom Right
                                      ]
                             ))

-- Draw a Sierpinski Carpet recursively given window, x, y and size
sierCarpet            :: Window -> Int -> Int -> Int -> IO()
sierCarpet w x y size = 
                       if size <= minimumSize     -- If size < minimumSize, draw the rectangle
                       then fillRectangle w x y size
                       else let sizeBy3 = size `div` 3
                                next    = 2 * sizeBy3
                            in do sierCarpet w x             y             sizeBy3
                                  sierCarpet w (x + sizeBy3) y             sizeBy3 
                                  sierCarpet w (x + next)    y             sizeBy3 
                                  sierCarpet w x             (y + sizeBy3) sizeBy3
                                  sierCarpet w (x + next)    (y + sizeBy3) sizeBy3 
                                  sierCarpet w x             (y + next)    sizeBy3
                                  sierCarpet w (x + sizeBy3) (y + next)    sizeBy3
                                  sierCarpet w (x + next)    (y + next)    sizeBy3

-- Drae the Sierpinski Carpet on a Window
sierpinskiCarpet :: IO ()
sierpinskiCarpet =  runGraphics(
                      do w <- openWindow "Sierpinski Carpet" (410,410)
                         sierCarpet w 10 10 400
                         k <- getKey w
                         closeWindow w
                    )
