{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies #-}
module Main
where

import Data.Maybe
import System.IO

import qualified Graphics.UI.SDL as SDL

import Graphics.Ogre.Ogre
import Common

initGame :: IO ()
initGame = do
    let set = OgreSettings "resources.cfg" False "Hogre Example 03" (Color 0.2 0.2 0.2) StencilModulative [ExteriorClose]
    let cam = Camera (Vector3 242.0 120.0 580) 0 (Vector3 241 120 580)
    let (wid, hei) = (1024, 768)
    let sce = OgreScene cam [] []
    _ <- SDL.setVideoMode wid hei 16 [SDL.OpenGL, SDL.HWAccel]
    SDL.wasInit [SDL.InitEverything] >>= print
    initOgre set
    addScene sce
    setSkyDome (Just ("Examples/CloudySky", 5))
    setWorldGeometry "terrain.cfg"

main :: IO ()
main = runWithSDL initGame

