-- | Arkanoid game implemented in Haskell.
module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

-- | Screen width.
width :: Int
width = 800

-- | Screen height.
height :: Int
height = 600

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

-- | Block information.
data BlockInfo = Block
  { blockPos :: Position -- ^ (x, y) block location.
  , blockCol :: Color -- ^ Block color.
  }

-- | Size of blocks.
blockSize :: (Float, Float)
blockSize = (20, 10)

-- | List of blocks.
type Blocks = [BlockInfo]

-- | Game status.
data GameStatus = Game
  { ballLoc :: Position -- ^ (x, y) ball location.
  , ballVel :: Position -- ^ (x, y) ball velocity.
  , playerLoc :: Float -- ^ Player x position.
  , playerVel :: Float -- ^ Player x velocity.
  , isPaused :: Bool -- ^ Pause indicator.
  , blocks :: Blocks -- ^ Blocks currently on screen.
  }

-- | Starting state of the game.
initialState :: GameStatus
initialState = Game
  { ballLoc = (0, -100)
  , ballVel = (7, -80)
  , playerLoc = 0
  , playerVel = 0
  , isPaused = False
  , blocks = foldl (\x y -> x ++ [Block
    { blockPos = (-200 + (fromIntegral (mod (truncate y) 15)) * 30, 100 - (fromIntegral (truncate (y / 15))) * 40)
    , blockCol = orange
    }]) [] [0..59]
  }

-- | Convert state into a picture.
render :: GameStatus -- ^ State that is being redered.
       -> Picture  -- ^ Picture that represents game state.
render game =
  pictures [ball, walls, mkPlayer, drawBlocks]
  where
      -- Ball.
      ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballSize
      ballColor = dark red
      ballSize = 5

      -- Walls.
      sideWall :: Float -> Picture
      sideWall offset = translate offset 0 $ color wallColor $ rectangleSolid 10 $ fromIntegral height
      topWall = translate 0 (fromIntegral height / 2) $ color wallColor $ rectangleSolid (fromIntegral width + 10) 10
      wallColor = greyN 0.5
      walls = pictures [sideWall (fromIntegral width / 2), sideWall (- fromIntegral width / 2), topWall]

      -- Player paddle.
      mkPlayer = translate (playerLoc game) (-250) $ color playerColor $ rectangleSolid 50 10
      playerColor = light $ light blue

      -- Blocks.
      drawBlocks = pictures $ foldl (\x y -> x ++ [uncurry translate (blockPos y) (color (blockCol y) (uncurry rectangleSolid blockSize))]) [] (blocks game)



-- | Update the ball position.
moveBall :: Float -- ^ Number of seconds since last update
          -> GameStatus -- ^ The initial game state
          -> GameStatus -- ^ Updated game state

moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old location and velocity
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New location
    x' = x + vx * seconds
    y' = y + vy * seconds

-- | Return whether paddle hit the wall
paddleWallCollision :: Float -> Bool
paddleWallCollision x = if x + 25 >= (fromIntegral width / 2) - 5 || x - 25 <= -(fromIntegral width / 2) + 5
                        then True
                        else False

-- | Update the paddle position.
movePaddle :: Float -- ^ Number of seconds since last update
            -> GameStatus -- ^ The initial game state
            -> GameStatus -- ^ Updated game state

movePaddle seconds game = game { playerLoc = x' }
  where
    -- Old location and velocity
    x = playerLoc game
    vx = playerVel game

    -- New location
    x' = if paddleWallCollision x
          then
            -- If collision with right and moving right, stop.
            if x + 25 >= fromIntegral width / 2 - 5 && vx > 0
              then
                x - 1
              -- If collision with right and not moving right, all good.
              else if x + 25 >= fromIntegral width / 2 - 5
                then
                  x + vx * seconds
                -- If collision with left and moving left, stop.
                else if x - 25 <= -(fromIntegral width / 2) + 5 && vx < 0
                  then
                    x + 1
                  -- If collision with left and not moving left, all good.
                  else
                    x + vx * seconds
          -- If no collision, all good.
          else
            x + vx * seconds

-- | Given position and radius of the ball, return whether
-- a collision with wall occured.
wallCollision :: Position -> Radius -> Bool
wallCollision (x, y) radius = topCollision || leftCollision || rightCollision
  where
    topCollision = y + 2 * radius > fromIntegral height / 2
    leftCollision = x - 2 * radius <= -fromIntegral width / 2
    rightCollision = x + 2 * radius >= fromIntegral width / 2

-- | Given position and radius of the ball, return whether
-- a collision with paddle occured.
paddleCollision :: GameStatus -> Position -> Radius -> Bool
paddleCollision game (x, y) radius =
                                y - radius <= -250
                                && x >= paddlePosition - 25
                                && x <= paddlePosition + 25
                              where
                                paddlePosition = playerLoc game

-- | Change velocity of the ball and destroy hit block.
blockCollision :: GameStatus -> GameStatus
blockCollision game = 
   game {
    ballVel = foldl (\acc y -> let 
      (xblock, yblock) = blockPos y
      (xball, yball) = ballLoc game 
      radius = 5
      in if 
        (xblock + 10 <= xball - radius &&
        yblock + 5 >= yball - radius &&
        yblock - 5 <= yball + radius) ||
        (xblock - 10 >= xball + radius &&
        yblock + 5 >= yball - radius &&
        yblock - 5 <= yball + radius)
      then (-fst acc, snd acc) else
        if 
          (yblock + 5 <= yball - radius &&
          xblock + 10 >= xball - radius &&
          xblock - 10 <= xball + radius) ||
          (yblock - 5 >= yball + radius &&
          xblock + 10 >= xball - radius &&
          xblock - 10 <= xball + radius)
        then (fst acc, -snd acc) else acc) (ballVel game) (blocks game)
    , blocks = foldl (\x y -> let 
      (xblock, yblock) = blockPos y
      (xball, yball) = ballLoc game 
      radius = 5
      in if (
        xblock + 10 >= xball - radius &&
        xblock - 10 <= xball + radius &&
        yblock + 5 >= yball - radius &&
        yblock - 5 <= yball + radius) 
        then x else x ++ [y]

    ) [] (blocks game)
  }

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

-- | Respond to key events.
handleKeys :: Event -> GameStatus -> GameStatus
-- For 'r' keypress, game is returned to it's initial state.
handleKeys (EventKey (Char 'r') Down _ _) game = initialState
-- For 'p' keypress, game is paused/unpaused.
handleKeys (EventKey (Char 'p') Down _ _) game = game { isPaused = not $ isPaused game }
-- For '<-' keypress, move paddle to left.
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = game { playerVel = playerVel game - 50 }
-- For '<-' release, stop the paddle.
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game = game { playerVel = playerVel game + 50 }
-- For '->' keypress, move paddle to left.
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = game { playerVel = playerVel game + 50 }
-- For '->' release, stop the paddle.
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game = game { playerVel = playerVel game - 50 }
-- All other inputs are ignored.
handleKeys _ game = game

-- | Update the game by moving the ball.
update :: Float -> GameStatus -> GameStatus
update seconds game = if isPaused game
                      then
                        game
                      else if y < -(fromIntegral height / 2 - 5)
                        then
                          error "You lose!"
                        else
                          paddleBounce . blockCollision $ wallBounce $ movePaddle seconds $ moveBall seconds game
                      where
                        (_, y) = ballLoc game

-- | Window creation.
main :: IO ()
main = play window background fps initialState render handleKeys update
