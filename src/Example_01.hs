module Main
where

import Graphics.Ogre.HOgre

import Common

-- based on basic tutorial 6 from Ogre Wiki.
-- http://www.ogre3d.org/tikiwiki/Basic+Tutorial+6&structure=Tutorials
main :: IO ()
main = generalSetup $ \root -> do

  smgr <- root_getSceneManager root "Scene Manager"
  cam <- sceneManager_getCamera smgr "PlayerCam"

  camera_setPosition_CameraPfloatfloatfloat cam 0 0 80
  camera_lookAt cam 0 0 (-300)

  _ <- addEntity smgr "ogrehead.mesh"

  -- set ambient light
  colourValue_with 0.5 0.5 0.5 1.0 (sceneManager_setAmbientLight smgr)

  -- create a light
  l <- sceneManager_createLight_SceneManagerPcharP smgr "MainLight"
  light_setPosition_LightPfloatfloatfloat l 20 80 50

  window <- root_getAutoCreatedWindow root
  render window root () nullHandler

