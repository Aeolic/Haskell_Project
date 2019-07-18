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
  myDrawer,
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
data Drawer = Drawer {cursor :: Cursor, lineLength :: Float, pic :: Picture, stack :: Stack}
  deriving(Show)
type Degrees = Int

myCursor = Cursor (0.0, 0.0) 0.0
myDrawer = Drawer myCursor 5.0 (Pictures [Blank]) []

makeDegrees :: Float -> Float
makeDegrees a = a `mod'` 360.0

-- generic function for rotation (rotates to left per default)
rotate :: Float -> Cursor -> Cursor
rotate angle c = Cursor (anchor c) (makeDegrees (orientation c + angle))

-- takes an angle
rotateLeft :: Float -> MyState ()
rotateLeft angle = do
  Drawer c l p s <- get
  let newC = DrawerM.rotate angle c
  put $ Drawer newC l p s

-- takes an angle
rotateRight :: Float -> MyState ()
rotateRight angle = do
  Drawer c l p s <- get
  let newC = DrawerM.rotate (360.0 - angle) c
  put $ Drawer newC l p s

-- move cursor forward depending current orientation and line length
moveForward :: MyState ()
moveForward = do
  Drawer c l p s <- get
  let angle = makeDegrees $ (orientation c) + 180.0
  let newAnchor = calcNewCoords angle l (anchor c)
  let newC = Cursor newAnchor (orientation c)
  put $ Drawer newC l p s

moveBackward :: MyState ()
moveBackward = do
  Drawer c l p s <- get
  let newAnchor = calcNewCoords (orientation c) l (anchor c)
  let newC = Cursor newAnchor (orientation c)
  put $ Drawer newC l p s

-- angle -> distance -> anchor point -> new point
calcNewCoords :: Float -> Float -> Point -> Point
calcNewCoords o l (x,y) = ((cos $ radians o) * l + x, (sin $ (radians o)) * l + y)

radians :: Float -> Float
radians d = d * (pi/180.0)

createLine :: Float -> Float -> Point -> Path
createLine o l p = [p, calcNewCoords o l p]

drawForward :: MyState ()
drawForward = do
             Drawer c l p s <- get
             let path = createLine (orientation c) l (anchor c)
             let newAnchor = last path
             put (Drawer (Cursor newAnchor $ orientation c) l (Pictures ((Line path):(getPics p)))s)


getPics :: Picture -> [Picture]
getPics (Pictures a) = a

drawBackward :: MyState ()
drawBackward = do
  Drawer c l p s <- get
  let path = createLine (orientation c) (-l) (anchor c)
  let newAnchor = last path
  put (Drawer (Cursor newAnchor $ orientation c) l (Pictures [p, Line path]) s)

pushPosition :: MyState ()
pushPosition = do
              Drawer c l p s <- get
              put $ Drawer c l p (c:s)

popPosition :: MyState ()
popPosition = do
              Drawer c l p s <- get
              put $ Drawer (head s) l p (tail s)

getPicture :: MyState Picture
getPicture = do
  d <- get
  put d
  return $ pic d

composePicture :: MyState Picture
composePicture = do
  drawForward
  rotateLeft 90.0
  drawForward
  getPicture


drawPicture :: Picture -> IO ()
drawPicture p = do
  display (InWindow "My Window" (800, 600) (10, 10)) white $ p
