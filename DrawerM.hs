module DrawerM (
  module Graphics.Gloss,
  Drawer (..),
  Cursor (..),
  rotateLeft,
  rotateRight,
  moveForward,
  moveBackward,
  drawForward,
  drawBackward,
  getPicture,
  drawPicture,
  makeMyDrawer,
  pushPosition,
  popPosition,
  getPics,
  MyState
  )

where
import Graphics.Gloss
import Control.Monad.State
import Data.Fixed

type MyState a = StateT Drawer IO a

type Stack = [Cursor]

data Cursor = Cursor {anchor :: Point, orientation :: Float}
  deriving (Show)
data ColorState = ColorState {isColor :: Bool, rgb :: Color, counter :: Integer}
  deriving(Show)
data Drawer = Drawer {cursor :: Cursor, lineLength :: Float, colorState :: ColorState, pic :: Picture, stack :: Stack}
  deriving(Show)
type Degrees = Int

initCursor = Cursor (0,0) 0
makeMyDrawer f = Drawer initCursor 5.0 (ColorState f black 0) (Pictures [Blank]) []

makeDegrees :: Float -> Float
makeDegrees a = a `mod'` 360

radians :: Float -> Float
radians d = d * (pi/180)

incrementColor :: ColorState -> ColorState
incrementColor (ColorState f rgb c) | f = let r = (sin $ radians (0.3* (fromIntegral c))) * 127 + 128
                                              g = (sin $ radians (0.3*(fromIntegral c)) + 2.0) * 127 + 128
                                              b = (sin $ radians (0.3*(fromIntegral c)) + 4.0) * 127 + 128
                                          in ColorState f (makeColorI (floor r) (floor g) (floor b) 255) (c+1)
                                    | otherwise = ColorState f rgb c

-- generic function for rotation (rotates to left per default)
rotate :: Float -> Cursor -> Cursor
rotate angle c = Cursor (anchor c) (makeDegrees (orientation c + angle))

rotateLeft :: Float -> MyState ()
rotateLeft angle = do
  Drawer c l cs p s <- get
  let newC = DrawerM.rotate angle c
  put $ Drawer newC l cs p s

rotateRight :: Float -> MyState ()
rotateRight angle = do
  Drawer c l cs p s <- get
  let newC = DrawerM.rotate (360 - angle) c
  put $ Drawer newC l cs p s

-- move cursor forward depending current orientation and line length
moveForward :: MyState ()
moveForward = do
  Drawer c l cs p s <- get
  let angle = makeDegrees $ (orientation c) + 180
  let newAnchor = calcNewCoords angle l (anchor c)
  let newC = Cursor newAnchor (orientation c)
  put $ Drawer newC l (incrementColor cs) p s

moveBackward :: MyState ()
moveBackward = do
  Drawer c l cs p s <- get
  let newAnchor = calcNewCoords (orientation c) l (anchor c)
  let newC = Cursor newAnchor (orientation c)
  put $ Drawer newC l cs p s

-- angle -> distance -> anchor point -> new point
calcNewCoords :: Float -> Float -> Point -> Point
calcNewCoords o l (x,y) = ((cos $ radians o) * l + x, (sin $ (radians o)) * l + y)

createLine :: Float -> Float -> Point -> Path
createLine o l p = [p, calcNewCoords o l p]

drawForward :: MyState ()
drawForward = do
             Drawer c l cs p s <- get
             let path = createLine (orientation c) l (anchor c)
             let newAnchor = last path
             let newPic = color (rgb cs) (Line path)
             put (Drawer (Cursor newAnchor $ orientation c) l (incrementColor cs) (Pictures (newPic:(getPics p))) s)

drawBackward :: MyState ()
drawBackward = do
  Drawer c l cs p s <- get
  let path = createLine (orientation c) (-l) (anchor c)
  let newAnchor = last path
  let newPic = color (rgb cs) (Line path)
  put (Drawer (Cursor newAnchor $ orientation c) l (incrementColor cs) (Pictures (newPic:(getPics p))) s)

pushPosition :: MyState ()
pushPosition = do
              Drawer c l cs p s <- get
              put $ Drawer c l cs p (c:s)

popPosition :: MyState ()
popPosition = do
              Drawer c l cs p s <- get
              put $ Drawer (head s) l cs p (tail s)

-- unwraps list of pictures from Picture
getPics :: Picture -> [Picture]
getPics (Pictures a) = a

-- unwraps the Picture from the state structure
getPicture :: MyState Picture
getPicture = do
  d <- get
  put d
  return $ pic d

drawPicture :: Picture -> IO ()
drawPicture p = do
  display (InWindow "My Window" (800, 600) (10, 10)) white $ p
