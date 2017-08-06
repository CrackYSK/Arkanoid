-- | Arkanoid game implemented in Haskell.
module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

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

-- | Number of frames to show per second.
fps :: Int
fps = 60

-- | Ball radius alias.
type Radius = Float

-- | Ball position alias.
type Position = (Float, Float)

-- | Game status.
data GameStatus = Game
  { ballLoc :: Position -- ^ (x, y) ball location.
  , ballVel :: Position -- ^ (x, y) ball velocity.
  , player :: Float           -- ^ player x position.
  } deriving Show

-- | Starting state of the game.
initialState :: GameStatus
initialState = Game
  { ballLoc = (0, 0)
  , ballVel = (7, -30)
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
      topWall = translate 0 200 $ color wallColor $ rectangleSolid 610 10
      wallColor = greyN 0.5
      walls = pictures [sideWall 300, sideWall (-300), topWall]

      -- Player paddle.
      mkPlayer = translate (player game) (-150) $ color playerColor $ rectangleSolid 50 10
      playerColor = light $ light blue

-- | Update the ball position
moveBall :: Float -- ^ Number of seconds since last update
          -> GameStatus -- ^ The initial game state
          -> GameStatus -- ^ Updated game state

moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations
    x' = x + vx * seconds
    y' = y + vy * seconds

-- | Given position and radius of the ball, return wether
-- a collision with wall occured.
wallCollision :: Position -> Radius -> Bool
wallCollision (x, y) radius = topCollision || leftCollision || rightCollision
  where
    topCollision = y + 2 * radius > fromIntegral height / 2
    leftCollision = x - 2 * radius <= -fromIntegral width / 2
    rightCollision = x + 2 * radius >= fromIntegral width / 2

-- | Given position and radius of the ball, return wether
-- a collision with paddle occured.
paddleCollision :: GameStatus -> Position -> Radius ->Bool
paddleCollision game (x, y) radius =
                                y - 2 * radius == -150 
                                && x >= paddlePosition - 25
                                && x <= paddlePosition + 25
                              where
                                paddlePosition = player game

-- | Detect collision with a paddle. Upon collision,
-- change the velocity of the ball to bounce it off.
paddleBounce :: GameStatus -> GameStatus
paddleBounce game = game { ballVel = (vx, vy') }
  where
    -- Radius.
    radius = 5

    -- The old velocity
    (vx, vy) = ballVel game

    vy' = if paddleCollision game (ballLoc game) radius
          then
            -vy
          else
            vy

-- | Detect collision with  wall. Upon collision,
-- update velocity of the ball to bounce it off.
wallBounce :: GameStatus -> GameStatus
wallBounce game = game { ballVel = (vx', vy') }
  where
    -- Radius.
    radius = 5

    -- The old velocity
    (vx, vy) = ballVel game

    -- Position of the ball
    (x, y) = ballLoc game

    -- Velocity update
    vy' = if wallCollision (x, y) radius &&
              y + 2 * radius > fromIntegral height / 2
          then
            -vy
          else
            vy

    vx' = if wallCollision (x, y) radius &&
            y + 2 * radius <= fromIntegral height / 2
          then
            -vx
          else
            vx


-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: ViewPort -> Float -> GameStatus -> GameStatus
update _ seconds = paddleBounce . wallBounce . moveBall seconds

-- | Window creation.
main :: IO ()
main = simulate window background fps initialState render update
