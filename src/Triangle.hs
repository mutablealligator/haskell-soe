module Triangle where
import SOE
import Play

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [Vertex]
           deriving Show
-- >
type Radius = Int
type Side   = Int
type Vertex = (Int, Int)

fillTri :: Window -> Int -> Int -> Int -> IO()
fillTri w x y size = drawInWindow w (withColor Blue
                     (polygon [(x,y), (x+size,y), (x,y-size)]))

minSize :: Int
minSize = 8

sTri :: Window -> Int -> Int -> Int -> IO()
sTri w x y size
    = if size <= minSize
      then fillTri w x y size
      else let size2 = size `div` 2
               in do sTri w x y size2
                     sTri w x (y-size2) size2
                     sTri w (x+size2) y size2

main = runGraphics(
       do w <- openWindow "Sierpinski's Triangle" (400, 400)
          sTri w 50 300 256
          spaceClose w
       )

spaceClose :: Window -> IO()
spaceClose w = do k <- getKey w
                  if k == ' ' then closeWindow w else spaceClose w
