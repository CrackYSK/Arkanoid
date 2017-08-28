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

-- | Size of ball.
ballSize :: Float
ballSize = 5

-- | Ball color.
ballColor :: Color
ballColor = dark red

-- | Number of frames to show per second.
fps :: Int
fps = 60

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
  , playerAcc :: Float -- ^ Player acceleration.
  , isPaused :: Bool -- ^ Pause indicator.
  , blocks :: Blocks -- ^ Blocks currently on screen.
  , gameStat :: Int -- ^ Game status indicator. 0 - still playing, 1 - won, -1 - lost.
  }

-- | Starting state of the game.
initialState :: GameStatus
initialState = Game
  { ballLoc = (0, -100)
  , ballVel = (25, -150)
  , playerLoc = 0
  , playerVel = 0
  , playerAcc = 150
  , isPaused = True
  , blocks = foldl (\x y -> x ++ [Block
    { blockPos = (-250 + (fromIntegral (mod (truncate y) 15)) * 35, 100 - (fromIntegral (truncate (y / 15))) * 40)
    , blockCol = if (mod (truncate y) 11) == 0 then greyN 0.5 else orange
    }]) [] [0..59]
  , gameStat = 0
  }

-- | Convert state into a picture.
render :: GameStatus -- ^ State that is being rendered.
       -> Picture  -- ^ Picture that represents game state.
render game =
  if gameStat game == 0 
    then (if isPaused game then pictures [ball, walls, mkPlayer, drawBlocks, pauseMsg]
      else pictures [ball, walls, mkPlayer, drawBlocks])
    else if gameStat game == -1 
      then (pictures
        [ translate (-300) 0 (color red (text "You lost!"))
        , translate (-250) (-150) (scale 0.3 1 (color blue (text "Press r for new game!")))
        ]) 
      else (pictures
        [ translate (-300) 0 (color green (text "You won!"))
        , translate (-250) (-150) (scale 0.3 1 (color blue (text "Press r for new game!")))
        ])
  where
      -- Ball.
      ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballSize

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
      drawBlocks = pictures $ foldl (\x y -> x ++
        [uncurry translate (blockPos y) (color (blockCol y) (uncurry rectangleSolid blockSize))])
        [] (blocks game)

      -- Pause information.
      pauseMsg = translate (-180) 150 $ scale 0.3 1 $ color blue $ text "Press p to play!"



-- | Update the ball position.
moveBall :: Float -- ^ Number of seconds since last update
          -> GameStatus -- ^ Initial game state
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
paddleWallCollision :: Float -- ^ x coordinate of the player.
                      -> Bool -- ^ Whether paddle hit the wall.
paddleWallCollision x = if x + 25 >= (fromIntegral width / 2) - 5 || x - 25 <= -(fromIntegral width / 2) + 5
                        then True
                        else False

-- | Update the paddle position.
movePaddle :: Float -- ^ Number of seconds since last update
            -> GameStatus -- ^ Initial game state
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

-- | Given position of the ball, return whether
-- a collision with wall occured.
wallCollision :: Position -- ^ Ball position.
                -> Bool -- ^ Whether ball hit the wall.
wallCollision (x, y) = topCollision || leftCollision || rightCollision
  where
    topCollision = y + 2 * ballSize > fromIntegral height / 2
    leftCollision = x - 2 * ballSize <= -fromIntegral width / 2
    rightCollision = x + 2 * ballSize >= fromIntegral width / 2

-- | Given position of the ball, return whether
-- a collision with paddle occured.
paddleCollision :: GameStatus -- ^ Initial game status.
                -> Position -- ^ Ball position.
                -> Bool -- ^ Whether ball hit the paddle.
paddleCollision game (x, y) =
                                y - ballSize <= -250
                                && y - 1 >= -250
                                && x >= paddlePosition - 25
                                && x <= paddlePosition + 25
                              where
                                paddlePosition = playerLoc game

-- | Change velocity of the ball and destroy hit block.
blockCollision :: GameStatus -- ^ Initial game status.
                -> GameStatus -- ^ Game status after collision.
blockCollision game = 
   game {
    ballVel = foldl (\acc y -> let 
      (xblock, yblock) = blockPos y
      (xball, yball) = ballLoc game
      in if 
        ((xblock + 9 < xball - ballSize && xblock + 11 > xball - ballSize) ||
        (xblock - 9 > xball + ballSize && xblock - 11 < xball + ballSize)) &&
        yblock + 6 > yball - ballSize &&
        yblock - 6 < yball + ballSize
      then (-fst acc, snd acc) else
        if 
          (yblock + 6 > yball - ballSize && yblock + 4 < yball - ballSize ||
          (yblock - 6 < yball + ballSize && yblock - 4 > yball + ballSize)) &&
          xblock + 11 > xball - ballSize &&
          xblock - 11 < xball + ballSize
        then (fst acc, -snd acc) else acc) (ballVel game) (blocks game)
    , blocks = foldl (\x y -> let 
      (xblock, yblock) = blockPos y
      bColor = blockCol y
      (xball, yball) = ballLoc game 
      in if (
        xblock + 11 > xball - ballSize &&
        xblock - 11 < xball + ballSize &&
        yblock + 6 > yball - ballSize &&
        yblock - 6 < yball + ballSize &&
        bColor == orange)
        then x else x ++ [y]

    ) [] (blocks game)
  }

-- | Detect collision with a paddle. Upon collision,
-- change the velocity of the ball to bounce it off.
paddleBounce :: GameStatus -- ^ Initial game status.
              -> GameStatus -- ^ Game status after the bounce.
paddleBounce game = game { ballVel = (vx', vy') }
  where

    -- The old velocity.
    (vx, vy) = ballVel game

    -- The old location.
    (ballLocX, _) = ballLoc game

    -- Player location.
    playerLocX = playerLoc game

    -- Player velocity.
    playerV = playerVel game

    vy' = if paddleCollision game (ballLoc game)
          then
            -vy
          else
            vy

    vx' = if paddleCollision game (ballLoc game)
          then
            vx + vx * (vx / (sqrt ((vx^2) + (vy^2)))) + playerV * 0.3
          else
            vx

-- | Detect collision with  wall. Upon collision,
-- update velocity of the ball to bounce it off.
wallBounce :: GameStatus -- ^ Initial game status.
            -> GameStatus -- ^ Game status after the bounce.
wallBounce game = game { ballVel = (vx', vy') }
  where

    -- The old velocity
    (vx, vy) = ballVel game

    -- Position of the ball
    (x, y) = ballLoc game

    -- Velocity update
    vy' = if wallCollision (x, y) &&
              y + 2 * ballSize > fromIntegral height / 2
          then
            -vy
          else
            vy

    vx' = if wallCollision (x, y) &&
            y + 2 * ballSize <= fromIntegral height / 2
          then
            -vx
          else
            vx

-- | Respond to key events.
handleKeys :: Event -- ^ Event to handle.
            -> GameStatus -- ^ Initial game status.
            -> GameStatus -- ^ Game status after the event.
-- For 'r' keypress, game is returned to it's initial state.
handleKeys (EventKey (Char 'r') Down _ _) game = initialState
-- For 'p' keypress, game is paused/unpaused.
handleKeys (EventKey (Char 'p') Down _ _) game = game { isPaused = not $ isPaused game }
-- For '←' keypress, move paddle to left.
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) game = game { playerVel = playerVel game - playerAcc game }
-- For '←' release, stop the paddle.
handleKeys (EventKey (SpecialKey KeyLeft) Up _ _) game = game { playerVel = playerVel game + playerAcc game }
-- For '→' keypress, move paddle to left.
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) game = game { playerVel = playerVel game + playerAcc game }
-- For '→' release, stop the paddle.
handleKeys (EventKey (SpecialKey KeyRight) Up _ _) game = game { playerVel = playerVel game - playerAcc game }
-- For '↑' keypress, increase playerAcc and y component of ballVel by 10.
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) game = game { playerAcc = playerAcc game + 10
                                                              , ballVel = (vx, vy')
                                                              }
                                                              where
                                                                (vx, vy) = ballVel game
                                                                vy' = if vy > 0 then vy + 2 else vy - 2
-- For '↓' kreypress, decrease playerAcc and y component of ballVel by 10.
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) game = game { playerAcc = playerAcc game - 10
                                                                , ballVel = (vx, vy')
                                                                }
                                                                where
                                                                  (vx, vy) = ballVel game
                                                                  vy' = if vy > 0 then vy - 2 else vy + 2
-- All other inputs are ignored.
handleKeys _ game = game

-- | Checks if there is any destroyable block left.
hasBlocks :: Blocks -- ^ List of block on screen.
          -> Bool -- ^ Indicator whether there are destroyable blocks left.
hasBlocks blocks = foldl (\x y -> if blockCol y == orange then True else x) False blocks

-- | Update the game by moving the ball.
update :: Float -- ^ Seconds since last update.
        -> GameStatus -- ^ Initial game status.
        -> GameStatus -- ^ New game status.
update seconds game = if isPaused game
                      then
                        game
                      else if not $ hasBlocks $ blocks game
                      then
                        game { gameStat = 1}
                      else if y < -(fromIntegral height / 2 - 5)
                        then
                          game { gameStat = -1}
                        else
                          paddleBounce . blockCollision $ wallBounce $ movePaddle seconds $ moveBall seconds game
                      where
                        (_, y) = ballLoc game

-- | Window creation.
main :: IO ()
main = play window background fps initialState render handleKeys update
