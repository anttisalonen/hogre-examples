{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies #-}
module Main
where

import System.IO

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

main :: IO ()
main = runWithSDL initGame (return ())

