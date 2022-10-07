-- main = putStrLn "Please enter a number: "
--        >> (readLn >>= (\n -> putStrLn (show (n+1))))


-- main :: IO ()
-- main = do
--      putStrLn "Please enter a number: "
--      n <- readLn
--      putStrLn (show (n+1))


import Control.Concurrent (threadDelay)
-- Assuming speed = 1 block per second for version 1.
-- Refresh rate of 1 second.
--------------------------------------------------------------------------------
-- TYPES!
type Vector = (Int, Int)

-- direction vector will be something like (x, y) where x and y can be 0, 1, -1
data State = State { position :: Vector, direction :: Vector } -- speed :: Int }
  deriving (Show)

data Env = Env { frame :: Vector } -- maxSpeed :: Int, chargeSpeed :: Int }
  deriving (Show)

--------------------------------------------------------------------------------
-- State
next :: Env -> State -> State
next (Env (width, height)) (State (posX, posY) (dirX, dirY)) =
  let posX' = posX + dirX
      posY' = posY + dirY
      hasCrossedTopEdge = posY' > height
      hasCrossedBottomEdge = posY' < 0
      hasCrossedLeftEdge = posX' < 0
      hasCrossedRightEdge = posX' > width
      dirXFinal = if hasCrossedLeftEdge || hasCrossedRightEdge then (-dirX) else dirX
      dirYFinal = if hasCrossedTopEdge || hasCrossedBottomEdge then (-dirY) else dirY
      posXFinal = if hasCrossedLeftEdge || hasCrossedRightEdge
                    then posX + dirXFinal
                    else posX'
      posYFinal = if hasCrossedTopEdge || hasCrossedBottomEdge
                    then posY + dirYFinal
                    else posY'
  in State {
    position = (posXFinal, posYFinal),
    direction = (dirXFinal, dirYFinal)
  }

--------------------------------------------------------------------------------
-- Draw
drawState :: Env -> State -> String
drawState env@(Env (_, height)) state =
  unlines $ reverse $ map (\row -> drawRow env state row) [-1..height+1]

drawRow :: Env -> State -> Int -> String
drawRow env@(Env (width, _)) state row = map (\col -> charAt env state (col, row)) [-1..width+1]

charAt :: Env -> State -> Vector -> Char
charAt (Env (width, height)) (State (posX, posY) _) (x, y)
  | (posX, posY) == (x, y) = 'o'
  | y < 0 || y > height = '-'
  | x < 0 || x > width = '|'
  | otherwise = ' '

--------------------------------------------------------------------------------
-- Animate!

-- Homework: before printing, let's clear the terminal.
-- Use hoogle.haskell.org or search google to check how to clear the terminal.
animate :: Env -> State -> IO ()
animate env state = putStr (drawState env state) >> threadDelay 1000000 >> animate env (next env state)

main = animate (Env (11, 5)) (State (1,1) (1,1))