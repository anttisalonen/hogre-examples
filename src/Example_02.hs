{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies #-}
module Main
where

import Data.Maybe
import System.IO
import Control.Concurrent
import Control.Monad (when, forever)

import qualified Graphics.UI.SDL as SDL

import Graphics.Ogre.Ogre

input :: Action -> IO (Maybe Action)
input ac = do
  events <- pollAllSDLEvents
  when (not (null events)) (print events)
  let nac@(_, ro, t, q) = foldl eventToAction ac events
  if q then return Nothing else doAction ro t >> return (Just nac)

pollAllSDLEvents :: IO [SDL.Event]
pollAllSDLEvents = go []
    where go l = do
                   e <- SDL.pollEvent
                   if e == SDL.NoEvent 
                     then return l 
                     else do
                       es <- pollAllSDLEvents
                       return (e:es)

type Action = (Bool, (Float, Float), (Float, Float, Float), Bool)

eventToAction :: Action -> SDL.Event -> Action
eventToAction (bt, ro, t, _) SDL.Quit = (bt, ro, t, True)
eventToAction (bt, ro, t, _) (SDL.KeyDown (SDL.Keysym SDL.SDLK_ESCAPE _ _)) = (bt, ro, t, True)
eventToAction (bt, ro, t, _) (SDL.KeyDown (SDL.Keysym SDL.SDLK_q      _ _)) = (bt, ro, t, True)
eventToAction (bt, ro, t@(x_, y_, z_), q) (SDL.KeyDown (SDL.Keysym k _ _)) = case k of
  SDL.SDLK_UP       -> (bt, ro, (x_, y_, z_ - 1.0), q)
  SDL.SDLK_DOWN     -> (bt, ro, (x_, y_, z_ + 1.0), q)
  SDL.SDLK_RIGHT    -> (bt, ro, (x_ + 1.0, y_, z_), q)
  SDL.SDLK_LEFT     -> (bt, ro, (x_ - 1.0, y_, z_), q)
  SDL.SDLK_PAGEDOWN -> (bt, ro, (x_, y_ - 1.0, z_), q)
  SDL.SDLK_PAGEUP   -> (bt, ro, (x_, y_ + 1.0, z_), q)
  _ -> (bt, ro, t, q)
eventToAction (bt, ro, t@(x_, y_, z_), q) (SDL.KeyUp   (SDL.Keysym k _ _)) = case k of
  SDL.SDLK_UP        -> (bt, ro, (x_, y_, z_ + 1.0), q)
  SDL.SDLK_DOWN      -> (bt, ro, (x_, y_, z_ - 1.0), q)
  SDL.SDLK_RIGHT     -> (bt, ro, (x_ - 1.0, y_, z_), q)
  SDL.SDLK_LEFT      -> (bt, ro, (x_ + 1.0, y_, z_), q)
  SDL.SDLK_PAGEDOWN  -> (bt, ro, (x_, y_ + 1.0, z_), q)
  SDL.SDLK_PAGEUP    -> (bt, ro, (x_, y_ - 1.0, z_), q)
  _ -> (bt, ro, t, q)
eventToAction (False, ro, t, q)        (SDL.MouseMotion _ _ _ _) = (False, ro, t, q)
eventToAction (True, (ya, pit), t, q) (SDL.MouseMotion _ _ abs_x abs_y) = (True, (ya - (0.005 * fromIntegral abs_x), pit - (0.005 * fromIntegral abs_y)), t, q)
eventToAction (_,  ro, t, q) (SDL.MouseButtonUp   _ _ SDL.ButtonRight) = (False, ro, t, q)
eventToAction (_,  ro, t, q) (SDL.MouseButtonDown _ _ SDL.ButtonRight) = (True, ro, t, q)
eventToAction (bt, ro, t, q) _ = (bt, ro, t, q)

resetRotation :: Action -> Action
resetRotation (bt, _, t, q) = (bt, (0, 0), t, q)

doAction :: (Float, Float) -> (Float, Float, Float) -> IO ()
doAction (ya, pit) (x_, y_, z_) = do
  rotateCamera (YPR ya 0 0)  World
  rotateCamera (YPR 0 pit 0) Local
  translateCamera (Vector3 x_ y_ z_)

initGame :: IO ()
initGame = do
    let set = OgreSettings "resources.cfg" False "Hogre Example 02" (Color 0.2 0.2 0.2) StencilModulative
    let cam = Camera (Vector3 100.0 10.0 40) 0 (Vector3 101 10 40)
    let lightdiffuse = Color 0.9 0.9 0.9
    let lightspecular = Color 0.8 0.8 0.8
    let spotrange = (0.0, degToRad 80.0)
    let l1 = Light "light1" lightdiffuse lightspecular (SpotLight (Vector3  0 15.0 0) (Vector3   1.0  (-1.0)   1.0) spotrange)
    let l2 = Light "light2" lightdiffuse lightspecular (SpotLight (Vector3 70 15.0 0) (Vector3 (-1.0) (-1.0)   1.0) spotrange)
    let l3 = Light "light3" lightdiffuse lightspecular (SpotLight (Vector3  0 15.0 100) (Vector3   1.0  (-1.0) (-1.0)) spotrange)
    let l4 = Light "light4" lightdiffuse lightspecular (SpotLight (Vector3 70 15.0 100) (Vector3 (-1.0) (-1.0) (-1.0)) spotrange)
    let plane = Plane unitY 0.0 70.0 100.0 20 20 5.0 5.0 unitZ "HelicopterBody"
    let pl = Entity "ground" (Vector3 35.0 0.0 50.0) plane False (Vector3 1.0 1.0 1.0)
    let swordmesh = "sword.mesh"
    let swordscale = Vector3 1.66 1.44 1.0
    let sword1 = Entity "sword1" (Vector3 35.0 3.0 4.2) (StdMesh swordmesh (YPR  halfPI  0.0 0.0)) True swordscale
    let sword2 = Entity "sword2" (Vector3 35.0 3.0 84.5)  (StdMesh swordmesh (YPR  halfPI  0.0 0.0)) True swordscale
    let lig = [l1, l2, l3, l4]
    let ents = [pl, sword1, sword2]
    let sce = OgreScene cam ents lig
    let (wid, hei) = (1024, 768)
    _ <- SDL.setVideoMode wid hei 16 [SDL.OpenGL, SDL.HWAccel]
    SDL.wasInit [SDL.InitEverything] >>= print
    initOgre set
    addScene sce
    addEntity (Entity "obj1" (Vector3 5.0 20.0 10.0) (StdMesh "hornet.mesh" (YPR 0.0 0.0 0.0)) True (Vector3 0.6 0.6 0.6))

shutdown :: IO ()
shutdown = do
    putStrLn "Shutting down..."
    cleanupOgre

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ runThreadedNonblocking 20 20 >> return ()

runThreadedNonblocking :: Int -> Int -> IO ()
runThreadedNonblocking renderinterval handleinterval = do
   initGame
   let ri = renderinterval * 1000
   let si = handleinterval * 1000
   rtid <- forkIO (forever (renderOgre >> threadDelay ri))
   loopThreaded si [rtid] (False, (0, 0), (0, 0, 0), False)

fullCleanup :: [ThreadId] -> IO () -> IO ()
fullCleanup tids cf = mapM_ killThread tids >> cf

loopThreaded :: Int -> [ThreadId] -> Action -> IO ()
loopThreaded si tids ac = do
  i <- input ac
  case i of
    Nothing  -> fullCleanup tids shutdown
    Just nac -> threadDelay si >> loopThreaded si tids (resetRotation nac)

