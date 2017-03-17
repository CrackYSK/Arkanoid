module Main where

import Graphics.Gloss

-- | Screen width.
width :: Int
width = 600

-- | Screen height.
height :: Int
height = 400

-- | Screen position.
offset :: Int
offset = 100

-- | Make game window.
window :: Display
window = InWindow "Arkanoid" (width, height) (offset, offset)

-- | Background color of screen.
background :: Color
background = black

-- | Game status.
data GameStatus = Game
  { ballLoc :: (Float, Float) -- ^ (x, y) ball location.
  , ballVel :: (Float, Float) -- ^ (x, y) ball velocity.
  , player :: Float           -- ^ player x position.
  } deriving Show

-- | Starting state of the game.
initialState :: GameStatus
initialState = Game
  { ballLoc = (0, 0)
  , ballVel = (1, -3)
  , player = 0
  }

-- | Convert state into a picture.
render :: GameStatus -- ^ State that is being redered.
       -> Picture  -- ^ Picture that represents game state.
render game =
  pictures [ball, walls, mkPlayer]
  where
	  -- Ball.
      ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballSize
      ballColor = dark red
      ballSize = 5

      -- Walls.
      sideWall :: Float -> Picture
      sideWall offset = translate offset 0 $ color wallColor $ rectangleSolid 10 400
      topWall = translate 0 195 $ color wallColor $ rectangleSolid 600 10
      wallColor = greyN 0.5
      walls = pictures [sideWall 295, sideWall (-295), topWall]

      -- Player paddle.
      mkPlayer = translate (player game) (-150) $ color playerColor $ rectangleSolid 50 10
      playerColor = light $ light blue

main :: IO ()
main = display window background $ render initialState
