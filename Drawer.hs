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

myCursor = Cursor (0.0, 0.0) 0.0
myDrawer = Drawer myCursor 70.0 (Pictures [Blank])

rotate :: Float -> Cursor -> Cursor
rotate angle c = Cursor (anchor c) ((orientation c + angle) `mod'` 360.0)

rotateLeft :: Float -> State Drawer ()
rotateLeft angle = state $ \(Drawer c l p) -> ((), Drawer (Drawer.rotate (360.0 - angle) c) l p)

rotateRight :: Float -> State Drawer ()
rotateRight angle = state $ \(Drawer c l p) -> ((), Drawer (Drawer.rotate angle c) l p)

moveForward :: State Drawer ()
--moveForward = state $ \(Drawer c l p) -> ((), Drawer (Cursor (move (orientation c) l (anchor c)) (orientation c)) l p)
moveForward = state $ \(Drawer c l p) -> ((), Drawer (Cursor (move (((orientation c)+180.0)`mod'`360.0) l (anchor c)) (orientation c)) l p)

moveBackward :: State Drawer ()
--moveBackward = state $ \(Drawer c l p) -> ((), Drawer (Cursor (move (((orientation c)+180.0)`mod'`360.0) l (anchor c)) (orientation c)) l p)
moveBackward = state $ \(Drawer c l p) -> ((), Drawer (Cursor (move (orientation c) l (anchor c)) (orientation c)) l p)

move :: Float -> Float -> Point -> Point
move o l (x,y) = ((sin $ radians (-o)) * l + x, (cos $ (radians (-o))) * l + y)

radians :: Float -> Float
radians d = d * (pi/180)

createLine :: Float -> Float -> Point -> Path
createLine o l p = [p, move o l p]

drawForward :: State Drawer ()
--drawForward = state $ \(Drawer c l p) -> ((), Drawer c l (Pictures [p,  Rotate (orientation c) (Line $ createLine (orientation c) l (anchor c))]))
drawForward = do
             Drawer c l p <- get
             let path = createLine (orientation c) l (anchor c)
             let newP = Rotate (orientation c) (Line path)
             let newCP = last path
             put (Drawer (Cursor newCP $ orientation c) l (Pictures [p, newP]))
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
  drawForward
  rotateLeft 90.0
  drawForward
  -- drawText "A"
  -- moveForward
  -- drawText "B"
  -- moveForward
  -- drawText "C"
  -- moveForward
  -- rotateLeft 30.0
  -- drawText "D"
  getPicture

drawPicture :: Picture -> IO ()
drawPicture p = display (InWindow "My Window" (300, 300) (10, 10)) white $ p

execDrawing = drawPicture $ fst $ runState composePicture myDrawer
