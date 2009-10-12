module Common(runWithSDL)
where

import Control.Monad (when, forever)
import System.IO
import Control.Concurrent

import qualified Graphics.UI.SDL as SDL

import Graphics.Ogre.Ogre

pollAllSDLEvents :: IO [SDL.Event]
pollAllSDLEvents = go []
    where go l = do
                   e <- SDL.pollEvent
                   if e == SDL.NoEvent 
                     then return l 
                     else do
                       es <- pollAllSDLEvents
                       return (e:es)

input :: Action -> IO () -> IO (Maybe Action)
input ac action = do
  events <- pollAllSDLEvents
  when (not (null events)) (print events >> (getCameraPosition >>= print))
  let nac@(_, ro, t, q) = foldl eventToAction ac events
  if q then return Nothing else doAction ro t action >> return (Just nac)

type Action = (Bool,        -- right mouse button pressed
    (Float, Float),         -- rotation (yaw, pitch)
    (Float, Float, Float),  -- translation (x, y, z)
    Bool)                   -- quit flag

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

doAction :: (Float, Float) -> (Float, Float, Float) -> IO () -> IO ()
doAction (ya, pit) (x_, y_, z_) act = do
  rotateCamera (YPR ya 0 0)  World
  rotateCamera (YPR 0 pit 0) Local
  translateCamera (Vector3 x_ y_ z_)
  act

shutdown :: IO ()
shutdown = do
    putStrLn "Shutting down..."
    cleanupOgre

runWithSDL :: IO () -> IO () -> IO ()
runWithSDL initGame action = SDL.withInit [SDL.InitEverything] $ runThreadedNonblocking initGame action 20 20 >> return ()

runThreadedNonblocking :: IO () -> IO () -> Int -> Int -> IO ()
runThreadedNonblocking initGame action renderinterval handleinterval = do
   initGame
   let ri = renderinterval * 1000
   let si = handleinterval * 1000
   rtid <- forkIO (forever (renderOgre >> threadDelay ri))
   loopThreaded si action [rtid] (False, (0, 0), (0, 0, 0), False)

fullCleanup :: [ThreadId] -> IO () -> IO ()
fullCleanup tids cf = mapM_ killThread tids >> cf

loopThreaded :: Int -> IO () -> [ThreadId] -> Action -> IO ()
loopThreaded si action tids ac = do
  i <- input ac action
  case i of
    Nothing  -> fullCleanup tids shutdown
    Just nac -> threadDelay si >> loopThreaded si action tids (resetRotation nac)

