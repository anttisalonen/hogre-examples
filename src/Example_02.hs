{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies #-}
module Main
where

import Data.Either
import System.IO
import Control.Monad (when)

import qualified Graphics.UI.SDL as SDL

import Graphics.Ogre.Ogre
import Game

data ExampleGame = ExampleGame { val :: Float }
    deriving (Eq, Show, Read)

data ExampleGameAction = Up
                       | Down
    deriving (Eq, Show, Read)

instance Game ExampleGame ExampleGameAction String where
    init _ = initGame
    cleanup = shutdown 
    render game = setEntityPosition "obj1" (Vector3 20 (val game) 20) >> renderOgre
    renew game = ExampleGame (val game + 0.1)
    process _ = []
    update game _ = game
    input _ = do
      events <- pollAllSDLEvents
      when (not (null events)) (print events)
      let evts = map handleEvent events
      -- when (not (null evts)) (print evts)
      if (not . null . lefts) evts 
        then return (Left (head (lefts (evts))))
        else return (Right (concat (rights evts)))

pollAllSDLEvents :: IO [SDL.Event]
pollAllSDLEvents = go []
    where go l = do
                   e <- SDL.pollEvent
                   if e == SDL.NoEvent 
                     then return l 
                     else do
                       es <- pollAllSDLEvents
                       return (e:es)

handleEvent :: SDL.Event -> Either String [ExampleGameAction]
handleEvent SDL.Quit                                  = Left "Quit"
handleEvent (SDL.KeyDown (SDL.Keysym SDL.SDLK_q _ _)) = Left "q"
handleEvent _                                         = Right [Up]

initGame :: IO ()
initGame = do
    let set = OgreSettings "resources.cfg" False "Ogre/Haskell Test" (Color 0.2 0.2 0.2) TextureModulative
    let cam = Camera (Vector3 35.0 0.0 50.0) halfPI (Vector3 35.0 100.0 50.0)
    let lightdiffuse = Color 0.9 0.9 0.9
    let lightspecular = Color 0.8 0.8 0.8
    let spotrange = (0.0, degToRad 80.0)
    let l1 = SpotLight "light1" (Vector3 0 15.0 0)    lightdiffuse lightspecular (Vector3   1.0  (-1.0)   1.0)  spotrange
    let l2 = SpotLight "light2" (Vector3 70 15.0 0)   lightdiffuse lightspecular (Vector3 (-1.0) (-1.0)   1.0)  spotrange
    let l3 = SpotLight "light3" (Vector3 0 15.0 100)  lightdiffuse lightspecular (Vector3   1.0  (-1.0) (-1.0)) spotrange
    let l4 = SpotLight "light4" (Vector3 70 15.0 100) lightdiffuse lightspecular (Vector3 (-1.0) (-1.0) (-1.0)) spotrange
    let plane = Plane unitY 0.0 70.0 100.0 20 20 5.0 5.0 unitZ "Examples/GrassFloor"
    let pl = Entity "ground" (Vector3 35.0 0.0 50.0) plane False (Vector3 1.0 1.0 1.0)
    let goalmesh = "Goal.mesh"
    let goalscale = Vector3 3.66 2.44 1.0
    let goal1 = Entity "goal1" (Vector3 35.0 1.0 (-4.2)) (StdMesh goalmesh (YPR  halfPI  0.0 0.0)) True goalscale
    let goal2 = Entity "goal2" (Vector3 35.0 1.0 104.5)  (StdMesh goalmesh (YPR (-halfPI) 0.0 pi)) True goalscale
    let lig = [l1, l2, l3, l4]
    let ents = [pl, goal1, goal2]
    let sce = OgreScene cam ents lig
    let (wid, hei) = (1024, 768)
    _ <- SDL.setVideoMode wid hei 32 [SDL.OpenGL, SDL.HWAccel]
    mapM_ putStrLn (replicate 10 "")
    SDL.wasInit [SDL.InitEverything] >>= print
    initOgre set
    addScene sce
    addEntity (Entity "obj1" (Vector3 5.0 20.0 10.0) (StdMesh "robot.mesh" (YPR 0.0 0.0 0.0)) True (Vector3 0.1 0.1 0.1))

shutdown :: ExampleGame -> IO ()
shutdown game = do
    print game
    putStrLn "Shutting down..."
    cleanupOgre

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ runThreadedNonblocking (ExampleGame 0) 20 20 >> return ()

