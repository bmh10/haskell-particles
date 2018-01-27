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
width = 765 -- 51 * 15
height = 465 + dashboardHeight -- 31 * 15
dashboardHeight = 20
offset = 100
particleRadius = 5
maxTileX = 50
maxTileY = 27
window = InWindow "Particles" (width, height) (offset, offset)
background = black

data LifeGame = Game
  { 
    particles :: [Particle],
    paused :: Bool
  } deriving Show 

data Particle = Particle
  {
    pos :: (Int, Int)
  } deriving Show

-- Rendering
render :: LifeGame -> Picture 
render g = pictures [renderParticles g, 
                     renderDashboard g]

renderDashboard :: LifeGame -> Picture
renderDashboard g = G2.color white $ translate (-300) (-fromIntegral height/2 + 5) $ scale 0.1 0.1 $ text $ "Dashboard"

renderParticles g = pictures $ map renderParticle (particles g)

renderParticle :: Particle -> Picture
renderParticle p
 = translate x' y' $ G2.color blue $ circleSolid particleRadius
  where
    (x, y) = pos p
    (x', y') = (fromIntegral x, fromIntegral y)
   

-- Event handling
handleKeys :: Event -> LifeGame -> LifeGame
handleKeys (EventKey (Char 'p') Down _ _) g = togglePaused g
handleKeys _ game = game

togglePaused   g = g { paused   = not (paused g) }

update :: Float -> LifeGame -> LifeGame
update secs game
 | (paused game) = game
 | otherwise     = updateGame game

updateGame g = g { particles = updateParticles (particles g) }

updateParticles [] = []
updateParticles (p:ps) = p { pos = add (pos p) (0, -1) } : updateParticles ps

add (a,b) (c,d) = (a+c,b+d)

initGame = do 
  let initialParticles = [Particle { pos = (10, 10) }, 
                          Particle { pos = (20, 20) }]
  let initialState = Game { paused = False, particles = initialParticles }
  return initialState

main = do
  initialState <- initGame
  play window background fps initialState render handleKeys update
