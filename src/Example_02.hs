{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies #-}
module Main
where

import qualified Graphics.UI.SDL as SDL

import Graphics.Ogre.Ogre
import Common

initGame :: IO ()
initGame = do
    let set = OgreSettings "resources.cfg" False "Hogre Example 02" (Color 0.2 0.2 0.2) StencilModulative [Generic]
    let cam = Camera (Vector3 100.0 10.0 40) 0 (Vector3 101 10 40)
    let lightdiffuse = Color 0.9 0.9 0.9
    let lightspecular = Color 0.8 0.8 0.8
    let spotrange = (0.0, degToRad 80.0)
    let l1 = Light "light1" lightdiffuse lightspecular (SpotLight (Vector3  0 15.0 0) (Vector3   1.0  (-1.0)   1.0) spotrange)
    let l2 = Light "light2" lightdiffuse lightspecular (SpotLight (Vector3 70 15.0 0) (Vector3 (-1.0) (-1.0)   1.0) spotrange)
    let l3 = Light "light3" lightdiffuse lightspecular (SpotLight (Vector3  0 15.0 100) (Vector3   1.0  (-1.0) (-1.0)) spotrange)
    let l4 = Light "light4" lightdiffuse lightspecular (SpotLight (Vector3 70 15.0 100) (Vector3 (-1.0) (-1.0) (-1.0)) spotrange)
    let plane = Plane unitY 0.0 70.0 100.0 20 20 5.0 5.0 unitZ "Lava"
    let pl = Entity "ground" (Vector3 35.0 0.0 50.0) plane False (Vector3 1.0 1.0 1.0)
    let robotmesh = "robot.mesh"
    let robotscale = Vector3 0.36 0.24 0.3
    let robot1 = Entity "robot1" (Vector3 35.0 3.0 4.2) (StdMesh robotmesh (YPR  halfPI  0.0 0.0)) True robotscale
    let robot2 = Entity "robot2" (Vector3 35.0 3.0 84.5)  (StdMesh robotmesh (YPR  halfPI  0.0 0.0)) True robotscale
    let lig = [l1, l2, l3, l4]
    let ents = [pl, robot1, robot2]
    let sce = OgreScene cam ents lig
    let (wid, hei) = (1024, 768)
    _ <- SDL.setVideoMode wid hei 16 [SDL.OpenGL, SDL.HWAccel]
    SDL.wasInit [SDL.InitEverything] >>= print
    initOgre set
    addScene sce
    addEntity (Entity "obj1" (Vector3 5.0 20.0 10.0) (StdMesh robotmesh (YPR 0.0 0.0 0.0)) True (Vector3 0.2 0.2 0.2))

main :: IO ()
main = runWithSDL initGame ((), EventCallback (\_ _ -> return ()), FrameCallback (\_ -> return ()))

