module Main where

import Graphics.Gloss
import qualified Graphics.Gloss.Game as GG
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game as G2
import System.IO  
import System.Random 
import Control.Monad
import Data.Fixed
import Data.List
import Data.Maybe

fps = 20
width = 800
height = 500 + dashboardHeight -- 31 * 15
dashboardHeight = 20
offset = 100

particleRadius = 3
gravityPerFrame = -2
xVelRange = 5

window = InWindow "Particles" (width, height) (offset, offset)
background = black

data LifeGame = Game
  { 
    particles :: [Particle],
    objects :: [Particle],
    paused :: Bool,
    particleCreationCol :: G2.Color,
    gen :: StdGen
  } deriving Show 

data Particle = Particle
  {
    pos :: (Int, Int),
    vel :: (Int, Int),
    mass :: Int,
    radius :: Float,
    col :: G2.Color
  } deriving Show

particle pos vel mass radius col = Particle { pos = pos, vel = vel, mass = mass, radius = radius, col = col }

randomParticles 0 gen col = ([], gen)
randomParticles n gen col = (p : ps, gen'')
  where (p, gen')   = randomParticle gen col
        (ps, gen'') = randomParticles (n-1) gen' col

randomParticle gen col = (particle (x, y) (xvel, mass*gravityPerFrame) mass particleRadius col, gen''')
  where (x, y, gen')   = randomPos gen
        (xvel, gen'')  = randomVel gen'
        (mass, gen''') = randomMass gen''

randomPos gen = (x, y, gen'')
  where (x, gen')  = randomR (-w, w)  gen
        (y, gen'') = randomR (h-50, h+50) gen'
        w = quot width 2
        h = quot height 2

randomVel gen  = randomR (-xVelRange, xVelRange) gen
randomMass gen = randomR (1, 5) gen

-- Rendering
render :: LifeGame -> Picture 
render g = pictures [renderParticles g, 
                     renderDashboard g]

renderDashboard :: LifeGame -> Picture
renderDashboard g = G2.color white $ translate (-300) (-fromIntegral height/2 + 5) $ scale 0.1 0.1 $ text $ "Particles: " ++ show (length (particles g))

renderParticles g = pictures $ map renderParticle (objects g) ++ map renderParticle (particles g)

renderParticle :: Particle -> Picture
renderParticle p
 = translate x' y' $ G2.color (col p) $ circleSolid (radius p)
  where
    (x, y) = pos p
    (x', y') = (fromIntegral x, fromIntegral y)
   

-- Event handling
handleKeys :: Event -> LifeGame -> LifeGame
handleKeys (EventKey (Char 'p') Down _ _) g = togglePaused g
handleKeys (EventKey (Char 'c') Down _ _) g = toggleParticleColor g
handleKeys _ game = game

togglePaused   g = g { paused   = not (paused g) }
toggleParticleColor g = g { particleCreationCol = nextCol (particleCreationCol g)}
  where nextCol c 
          | c == blue  = red
          | c == red   = green
          | c == green = blue

update :: Float -> LifeGame -> LifeGame
update secs game
 | (paused game) = game
 | otherwise     = updateGame game

updateGame g = addParticles $ g { particles = updateParticles (particles g) (objects g) }

addParticles g = g { particles = (particles g) ++ ps, gen = gen' }
  where (ps, gen') = randomParticles 10 (gen g) (particleCreationCol g)

updateParticles [] _ = []
updateParticles (p:ps) os = updateParticle p os ++ updateParticles ps os
  where updateParticle p os
          | willHitObject p os = [p] -- TODO
          | willHitWall p = [p { vel = bounceX (vel p) }]
          | inRange p = [p { pos = add (pos p) (vel p) }]
          | otherwise = []

bounceX (x, y) = (-x, y)

willHitObject _ [] = False
willHitObject p (o:os) = willHitObject' p o || willHitObject p os
  where willHitObject' p o = False -- TODO

willHitWall p = x < -w || x > w
  where (x, y) = add (pos p) (vel p)
        w = quot width 2

inRange p = -w <= x && x <= w && -h <= y && y <= h
  where (x, y) = pos p
        w = quot width 2
        h = quot height 2

add (a,b) (c,d) = (a+c,b+d)

initGame = do 
  stdGen <- newStdGen
  let (initialParticles, stdGen') = randomParticles 100 stdGen blue
  let initialObjects = [Particle { pos = (0, 0), vel = (0, 0), mass = 1 , radius = 20, col = red}]
  let initialState = Game { paused = False, particles = initialParticles, objects = initialObjects, gen = stdGen', particleCreationCol = blue }
  return initialState

main = do
  initialState <- initGame
  play window background fps initialState render handleKeys update
