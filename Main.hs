module Main where

import Graphics.Gloss
import qualified Graphics.Gloss.Game as GG
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game as G2
import System.IO  
import Control.Monad
import Data.Fixed
import Data.List
import Data.Maybe

fps = 20
numSeeds = 4
width = 765 -- 51 * 15
height = 465 + dashboardHeight -- 31 * 15
dashboardHeight = 20
offset = 100
tileSize = 15
maxTileX = 50
maxTileY = 27
dimPerFrame = 0.05
window = InWindow "Particles" (width, height) (offset, offset)
background = black

data LifeGame = Game
  { 
    allLevels :: [[[Float]]],
    currentLevel :: [[Float]],
    currentLevelIdx :: Int,
    levelDescriptions :: [String],
    paused :: Bool,
    wrapping :: Bool,
    dimming :: Bool,
    colour :: G2.Color,
    aliveCount :: Int,
    prevAliveCount :: Int
  } deriving Show 

-- Tile functions
isTileAlive x y g 
 = ((wrapping g) || inRangeXY x y) && (getTileWrapped x y g > 0)

numNeighbours x y g = length $ filter (==True) $ map (\(x,y) -> isTileAlive x y g) $ neighbours x y 

neighbours x y = [(x-1, y-1), (x-1, y), (x-1, y+1), (x, y-1), (x, y+1), (x+1, y-1), (x+1, y), (x+1, y+1)]

getTileWrapped x y g = getTile wx wy g
  where
    wx = wrapx x
    wy = wrapy y

getTile :: Int -> Int -> LifeGame -> Float
getTile x y g = (currentLevel g) !! y !! x

setTileDead x y g = setTile x y 0 g
setTileAlive x y g = setTile x y 1 g
dimTile x y g = setTile x y alpha' g
  where
    alpha  = getTile x y g
    alpha' = if (dimming g) then alpha - dimPerFrame else alpha

setTile :: Int -> Int -> Float -> LifeGame -> LifeGame
setTile x y c g = g { currentLevel = updatedLevel}
  where updatedLevel = setAtIdx y (setAtIdx x c ((currentLevel g) !! y)) (currentLevel g)

-- Map tile coords ((0,0) is top-left tile) to actual screen coords ((0, 0) is center of screen)
tileToCoord :: (Int, Int) -> (Float, Float) 
tileToCoord (x, y) = (fromIntegral x*tileSize + tileSize/2 - fromIntegral width/2, fromIntegral height/2 - fromIntegral y*tileSize - tileSize/2)

setAtIdx :: Int -> a -> [a] -> [a]
setAtIdx idx val xs = take idx xs ++ [val] ++ drop (idx+1) xs

getAliveCount g = length $ filter (>0) $ concat (currentLevel g)

-- Rendering
render :: LifeGame -> Picture 
render g = pictures [renderLevel g, 
                     renderDashboard g]

renderDashboard :: LifeGame -> Picture
renderDashboard g = G2.color white $ translate (-300) (-fromIntegral height/2 + 5) $ scale 0.1 0.1 $ text $ seedNum ++ wrapAround ++ dimmingEnabled ++ isPaused ++ fpsCount ++ aliveCells ++ isStable
  where
    fpsCount = " FPS: " ++ show fps
    aliveCells = " Alive: " ++ show (aliveCount g)
    isPaused  = " Pause: " ++ show (paused g)
    wrapAround  = " Wrap: " ++ show (wrapping g)
    dimmingEnabled  = " Dim: " ++ show (dimming g)
    seedNum  = " Seed: " ++ show ((currentLevelIdx g)+1) ++ ": " ++ show ((levelDescriptions g) !! (currentLevelIdx g))
    isStable = " Stable: " ++ show ((aliveCount g) == (prevAliveCount g))

renderLevel :: LifeGame -> Picture
renderLevel game = renderLines (currentLevel game) 0 (colour game)

renderLines :: [[Float]] -> Int -> G2.Color -> Picture
renderLines [] _ _ = blank
renderLines (l:ls) y c = pictures [renderLine l 0 y c, renderLines ls (y+1) c]

renderLine :: [Float] -> Int -> Int -> G2.Color -> Picture
renderLine [] _ _ _      = blank
renderLine (t:ts) x y c  = pictures [renderTile t x y c, renderLine ts (x+1) y c]

renderTile :: Float -> Int -> Int -> G2.Color -> Picture
renderTile c x y col
 | c > 0  = translate x' y' $ G2.color col' $ rectangleSolid (tileSize-1) (tileSize-1)
 | otherwise = blank
  where
    (x', y') = tileToCoord (x, y)
    (r, g, b, a) = rgbaOfColor col
    col' = makeColor r g b c

-- Event handling
handleKeys :: Event -> LifeGame -> LifeGame
handleKeys (EventKey (Char 'p') Down _ _) g = togglePaused g
handleKeys (EventKey (Char 'w') Down _ _) g = toggleWrapping g
handleKeys (EventKey (Char 'd') Down _ _) g = toggleDimming g
handleKeys (EventKey (Char 'r') Down _ _) g = resetLevel g
handleKeys (EventKey (Char 'c') Down _ _) g = nextColour g
handleKeys (EventKey (Char 'n') Down _ _) g = nextLevel g
handleKeys _ game = game

update :: Float -> LifeGame -> LifeGame
update secs game
 | (paused game)               = game
 | otherwise                   = updateState $ updateLevel game

updateState :: LifeGame -> LifeGame
updateState g = g { aliveCount = (getAliveCount g), prevAliveCount = (aliveCount g) }

updateLevel :: LifeGame -> LifeGame
updateLevel g = foldr (\(x,y) -> updateCell x y g) g' coords
  where
    coords = [(x, y) | x <- [0..maxTileX], y <- [0..maxTileY]]
    g' = g -- g is initial/current state, g' is next/updated state

updateCell x y g g'
 | live && neighbours < 2 = setTileDead x y g'
 | live && neighbours >= 2 && neighbours <= 3 = dimTile x y g'
 | live && neighbours > 3 = setTileDead x y g'
 | not live && neighbours == 3 = setTileAlive x y g'
 | otherwise = dimTile x y g'
  where 
    live       = isTileAlive x y g
    neighbours = numNeighbours x y g

wrapx x = wrap x maxTileX
wrapy y = wrap y maxTileY
wrap p max
 | p < 0 = max
 | p > max = 0
 | otherwise = p

inRangeXY x y = inRange x maxTileX && inRange y maxTileY
inRange p max = p >= 0 && p <= max

togglePaused   g = g { paused   = not (paused g) }
toggleWrapping g = g { wrapping = not (wrapping g) }
toggleDimming  g = g { dimming  = not (dimming g) }

nextLevel g = resetLevel $ g { currentLevelIdx = wrap ((currentLevelIdx g)+1) (numSeeds-1) }

resetLevel g = g { currentLevel = (allLevels g) !! (currentLevelIdx g) }

nextColour g  = g { colour = incColour (colour g)}
  where 
    incColour c
      | c == blue  = red
      | c == red   = green
      | c == green = blue 

levelToAlpha :: [String] -> [[Float]]
levelToAlpha xs = map toAlpha xs

toAlpha :: String -> [Float]
toAlpha [] = []
toAlpha (c:cs) = (if c == '_' then 0 else 1) : toAlpha cs

initTiles = do 
  let fileNames = map (\x -> "seed" ++ show x ++ ".txt") [1..numSeeds]
  fileContents <- mapM readFile fileNames
  descriptionContents <- readFile "seedDescriptions.txt"
  let all = map (levelToAlpha . words) fileContents
  let descriptions = lines descriptionContents
 
  let initialState = Game { allLevels = all, currentLevel = all !! 0, currentLevelIdx = 0, levelDescriptions = descriptions, paused = False, wrapping = True, dimming = False, colour = blue, aliveCount = 0, prevAliveCount = 0 }
  print all
  return initialState

main = do
  initialState <- initTiles
  play window background fps initialState render handleKeys update
