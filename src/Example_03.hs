{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies #-}
module Main
where

import Control.Monad (when)

import qualified Graphics.UI.SDL as SDL

import Graphics.Ogre.Ogre
import Common

robotTemplate :: String -> Vector3 -> Entity
robotTemplate swname pos = Entity swname pos (StdMesh "robot.mesh" (YPR  halfPI  0.0 0.0)) True (Vector3 0.2 0.2 0.2)

initGame :: IO ()
initGame = do
    let set = OgreSettings "resources.cfg" False "Hogre Example 03" (Color 0.9 0.9 0.9) StencilModulative [ExteriorClose]
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
main = runWithSDL initGame (0, EventCallback query, FrameCallback (\v -> return v))

getLMB :: IO (Maybe (Int, Int))
getLMB = do
  (xpos, ypos, btns) <- SDL.getMouseState
  return $ case SDL.ButtonLeft `elem` btns of
    True  -> Just (xpos, ypos)
    False -> Nothing

getLMBPressed :: [SDL.Event] -> Maybe (Int, Int)
getLMBPressed []     = Nothing
getLMBPressed (e:es) = case e of
                         (SDL.MouseButtonDown xp yp SDL.ButtonLeft) -> Just ((fromIntegral xp), (fromIntegral yp))
                         _                                          -> getLMBPressed es

query :: Int -> [SDL.Event] -> IO Int
query counter events = do
  cameraAboveGround
  putObject counter events

cameraAboveGround :: IO ()
cameraAboveGround = do
  (Vector3 camx camy camz) <- getCameraPosition
  mres <- raySceneQuerySimple (Vector3 camx 5000 camz) negUnitY
  case mres of
    Nothing  -> return ()
    Just res -> when ((y res) + 10 > camy) $ setCameraPosition (Vector3 camx ((y res) + 10) camz)

putObject :: Int -> [SDL.Event] -> IO Int
putObject counter events = do
  let mp = getLMBPressed events
  case mp of
    Nothing           -> return counter
    Just (xpos, ypos) -> do
      scr <- SDL.getVideoSurface
      let wid = SDL.surfaceGetWidth scr
      let hei = SDL.surfaceGetHeight scr
      print (xpos, ypos, wid, hei)
      let wabs = (fromIntegral xpos) / (fromIntegral wid)
      let habs = (fromIntegral ypos) / (fromIntegral hei)
      pres <- raySceneQueryMouseSimple wabs habs
      case pres of
        Nothing                       -> return counter
        Just (Vector3 resx resy resz) -> do
          let swname = "robot" ++ (show counter)
          let pos = Vector3 resx (resy + 10) resz
          addEntity (robotTemplate swname pos)
          print swname
          return (counter + 1)

