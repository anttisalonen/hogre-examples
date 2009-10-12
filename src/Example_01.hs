module Main
where

import Graphics.Ogre.Ogre
import Control.Exception
import Control.Concurrent (threadDelay)

renderLoop :: Float -> IO ()
renderLoop f = do
    renderOgre
    setEntityPosition "obj1" (ogreloc f)
    setLightPosition "light2" (Vector3 30 20 50)
    threadDelay 10000
    renderLoop (f + 0.008)

ogreloc :: Float -> Vector3
ogreloc f = Vector3 (30 + 20 * sin f) (8 + 2 * sin (0.2 * f)) (50 + 20 * cos f)

plloc :: Float -> Vector3
plloc f = Vector3 (30 + 40 * sin (4.0 * f)) 20 (50 + 40 * cos (4.0 * f))

main :: IO ()
main = do
    let set = OgreSettings "resources.cfg" True "Hogre Example 01" (Color 0.2 0.2 0.2) StencilAdditive [Generic]
    let cam = Camera (Vector3 35.0 0.0 50.0) 0 (Vector3 135.0 40.0 50.0)
    let yelcol = Color 1.0 1.0 0.0
    let dircol = Color 0.25 0.25 0
    let whitecol = Color 1 1 1
    let spotrange = (0.0, degToRad 80.0)
    let l1 = Light "light1" yelcol yelcol (SpotLight (Vector3  30 50.0 50) (Vector3  0.0  (-1.0)   0.0) spotrange)
    let l2 = Light "light2" whitecol whitecol (PointLight (plloc 0))
    let l3 = Light "light3" dircol dircol (DirectionalLight (Vector3 0 0 1))
    let plane = Plane unitY 0.0 70.0 100.0 20 20 5.0 5.0 unitZ "DarkBlue"
    let pl = Entity "ground" (Vector3 35.0 0.0 50.0) plane False (Vector3 1.0 1.0 1.0)
    let lig = [l1, l2, l3]
    let ents = [pl]
    let sce = OgreScene cam ents lig
    initOgre set
    addScene sce
    addEntity (Entity "obj1" (ogreloc 0) (StdMesh "hornet.mesh" (YPR 0 halfPI 0)) True (Vector3 0.5 0.5 0.5))
    handle shutdown (renderLoop 0)

shutdown :: (Show a) => a -> IO ()
shutdown e = do
    print e
    putStrLn "Shutting down..."
    cleanupOgre

