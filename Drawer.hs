module Drawer (
  module Graphics.Gloss,
  Drawer,
  Cursor,
  rotateLeft,
  rotateRight,
  moveForward,
  moveBackward,
  drawForward,
  drawBackward,
  drawText,
  getPicture,
  drawPicture,
  execDrawing,
  myDrawer
  )


where
import Graphics.Gloss
import Control.Monad.State
import Data.Fixed

data Cursor = Cursor {anchor :: Point, orientation :: Float}
  deriving (Show)
data Drawer = Drawer {cursor :: Cursor, lineLength :: Float, pic :: Picture}
  deriving(Show)
type Degrees = Integer

myCursor = Cursor (0.0, 0.0) 0.0
myDrawer = Drawer myCursor 70.0 (Pictures [Blank])

makeDegrees :: Float -> Float
makeDegrees a = a `mod'` 360.0

rotate :: Float -> Cursor -> Cursor
rotate angle c = Cursor (anchor c) (makeDegrees (orientation c + angle))

rotateLeft :: Float -> State Drawer ()
rotateLeft angle = state $ \(Drawer c l p) -> ((), Drawer (Drawer.rotate angle c) l p)

rotateRight :: Float -> State Drawer ()
rotateRight angle = state $ \(Drawer c l p) -> ((), Drawer (Drawer.rotate (360.0 - angle) c) l p)

moveForward :: State Drawer ()
moveForward = state $ \(Drawer c l p) -> ((), Drawer (Cursor (move (makeDegrees ((orientation c)+180.0)) l (anchor c)) (orientation c)) l p)

moveBackward :: State Drawer ()
moveBackward = state $ \(Drawer c l p) -> ((), Drawer (Cursor (move (orientation c) l (anchor c)) (orientation c)) l p)

move :: Float -> Float -> Point -> Point
move o l (x,y) = ((cos $ radians o) * l + x, (sin $ (radians o)) * l + y)

radians :: Float -> Float
radians d = d * (pi/180.0)

createLine :: Float -> Float -> Point -> Path
createLine o l p = [p, move o l p]

drawForward :: State Drawer ()
--drawForward = state $ \(Drawer c l p) -> ((), Drawer c l (Pictures [p,  Rotate (orientation c) (Line $ createLine (orientation c) l (anchor c))]))
drawForward = do
             Drawer c l p <- get
             let path = createLine (orientation c) l (anchor c)
             let newAnchor = last path
             put (Drawer (Cursor newAnchor $ orientation c) l (Pictures [p, Line path]))
             return ()




drawBackward :: State Drawer ()
drawBackward = undefined

drawText :: String -> State Drawer ()
drawText s = state $ \(Drawer c l p) -> ((), Drawer c l (Pictures [p, Translate (fst (anchor c)) (snd (anchor c)) $ Rotate (orientation c) (Text s)]))

getPicture :: State Drawer Picture
getPicture = state $ \d -> ((pic d), Drawer (cursor d) (lineLength d) (pic d))

composePicture :: State Drawer Picture
composePicture = do
  drawForward
  rotateLeft 90.0
  drawForward
  -- rotateLeft 90.0
  -- drawForward
  -- rotateLeft 90.0
  -- drawForward
  getPicture

drawPicture :: Picture -> IO ()
drawPicture p = display (InWindow "My Window" (300, 300) (10, 10)) white $ p

execDrawing = drawPicture $ fst $ runState composePicture myDrawer
