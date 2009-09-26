module Main
where

import Graphics.Ogre.Ogre
import Control.Exception
import Control.Concurrent (threadDelay)

renderLoop :: Float -> IO ()
renderLoop f = do
    renderOgre
    setEntityPosition "obj1" (ogreloc f)
    threadDelay 10000
    renderLoop (f + 0.008)

ogreloc :: Float -> Vector3
ogreloc f = Vector3 (30 + 30 * sin f) (5 + 2 * sin (0.2 * f)) (50 + 30 * cos f)

main :: IO ()
main = do
    let set = OgreSettings "resources.cfg" True "Hogre Example 01" (Color 0.2 0.2 0.2) StencilAdditive
    let cam = Camera (Vector3 35.0 0.0 50.0) 0 (Vector3 135.0 40.0 50.0)
    let lightdiffuse = Color 0.9 0.9 0.9
    let lightspecular = Color 0.8 0.8 0.8
    let spotrange = (0.0, degToRad 80.0)
    let l1 = SpotLight "light1" (Vector3 0 15.0 0)    lightdiffuse lightspecular (Vector3   1.0  (-1.0)   1.0)  spotrange
    let l2 = SpotLight "light2" (Vector3 70 15.0 0)   lightdiffuse lightspecular (Vector3 (-1.0) (-1.0)   1.0)  spotrange
    let plane = Plane unitY 0.0 70.0 100.0 20 20 5.0 5.0 unitZ "Examples/Rocky"
    let pl = Entity "ground" (Vector3 35.0 0.0 50.0) plane False (Vector3 1.0 1.0 1.0)
    let lig = [l1, l2]
    let ents = [pl]
    let sce = OgreScene cam ents lig
    initOgre set
    addScene sce
    addEntity (Entity "obj1" (ogreloc 0) (StdMesh "ogrehead.mesh" (YPR 0 0 pi)) True (Vector3 0.1 0.1 0.1))
    handle shutdown (renderLoop 0)

shutdown :: (Show a) => a -> IO ()
shutdown e = do
    print e
    putStrLn "Shutting down..."
    cleanupOgre

