module MyFractal where
import SOE
import Play

data Coordinate = Coordinate Int Int

-- Define Minimum Size
minimumSize :: Int
minimumSize = 5

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

getXCoordinate :: Coordinate -> Int
getXCoordinate (Coordinate x _) = x

getYCoordinate :: Coordinate -> Int
getYCoordinate (Coordinate _ x) = x

-- Draw a Rectangle with Blue Color and specified co-ordinates
drawRectangle :: Window -> Coordinate ->  Coordinate  -> Coordinate -> Coordinate -> IO()
drawRectangle w (Coordinate x1 y1) (Coordinate x2 y2) (Coordinate x3 y3) (Coordinate x4 y4) =
                              drawInWindow w ( withColor Blue
                                (polygon [(x1, y1),
                                          (x2, y2),
                                          (x3, y3),
                                          (x4, y4)
                                         ]
                                ))

drawTrunk :: Window -> Int -> Int -> Int -> IO()
drawTrunk w x y size = drawRectangle
                        w
                        (Coordinate (x - newSize) y)
                        (Coordinate (x - newSize) (y - size))
                        (Coordinate (x + newSize) (y - size))
                        (Coordinate (x + newSize) y)
                        where
                            newSize = size `div` 2


pythTree :: Window -> Int -> Int -> Int -> IO()
pythTree w x y size =
                      if size <= minimumSize     -- If size < minimumSize, draw the rectangle
                          then drawTrunk w  x             y         size
                      else let newSize = size `div` 2
                               newDim  = newSize * isqrt 2
                      in do    drawTrunk w  x             y         size
                               pythTree  w (x - newSize) (y - size) newSize
                               pythTree  w (x + newSize) (y - size) newSize



myTest           :: IO()
myTest           =  runGraphics(
                       do
                                  w <- openWindow "My Fractal" (410,410)
                                  pythTree w 200 400 150
                                  k <- getKey w
                                  closeWindow w
                    )
