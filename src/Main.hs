module Main
where

import Graphics.Ogre.Ogre
import Control.Exception

renderLoop :: Float -> IO ()
renderLoop f = do
    renderOgre
    setEntityPosition "obj1" (Vector3 20 f 30)
    renderLoop (f + 0.015)

main :: IO ()
main = do
    let set = OgreSettings "resources.cfg" True "Ogre/Haskell Test" (Color 0.2 0.2 0.2) StencilAdditive
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
    initOgre set
    addScene sce
    addEntity (Entity "obj1" (Vector3 5.0 20.0 10.0) (StdMesh "robot.mesh" (YPR 0.0 0.0 0.0)) True (Vector3 0.1 0.1 0.1))
    handle shutdown (renderLoop 0)

shutdown :: (Show a) => a -> IO ()
shutdown e = do
    print e
    putStrLn "Shutting down..."
    cleanupOgre

