module Main
where

import System.Exit
import Control.Monad

import Graphics.Ogre.HOgre
import Graphics.Ogre.Types

-- based on basic tutorial 6 from Ogre Wiki.
-- http://www.ogre3d.org/tikiwiki/Basic+Tutorial+6&structure=Tutorials
main :: IO ()
main = do
  -- construct Ogre::Root
  root <- root_new "plugins.cfg" "ogre.cfg" "Ogre.log"

  -- setup resources
  root_addResourceLocation root "Media" "FileSystem" "Group" True

  -- configure
  -- show the configuration dialog and initialise the system
  restored <- root_restoreConfig root
  when (not restored) $ do
    configured <- root_showConfigDialog root
    when (not configured) $ exitWith (ExitFailure 1)
  window <- root_initialise root True "Render Window" ""

  -- set default mipmap level (some APIs ignore this)
  root_getTextureManager root >>= \tmgr -> textureManager_setDefaultNumMipmaps tmgr 5

  -- initialise all resource groups
  resourceGroupManager_getSingletonPtr >>= resourceGroupManager_initialiseAllResourceGroups

  -- create the scene manager, here a generic one
  smgr <- root_createSceneManager1 root "DefaultSceneManager" ""

  -- create and position the camera
  cam <- sceneManager_createCamera smgr "PlayerCam"
  camera_setPosition1 cam 0 0 80
  camera_lookAt cam 0 0 (-300)
  frustum_setNearClipDistance (toFrustum cam) 5

  -- create one viewport, entire window
  vp <- renderTarget_addViewport (toRenderTarget window) cam 0 0 0 1 1
  bgcolour <- colourValue_new 0 0 0 1
  viewport_setBackgroundColour vp bgcolour

  -- Alter the camera aspect ratio to match the viewport
  vpw <- viewport_getActualWidth vp
  vph <- viewport_getActualHeight vp
  frustum_setAspectRatio (toFrustum cam) (fromIntegral vpw / fromIntegral vph)

  ogreHead <- sceneManager_createEntity2 smgr "ogrehead.mesh"
  rootNode <- sceneManager_getRootSceneNode smgr
  headNode <- sceneManager_createSceneNode smgr ""
  node_addChild (toNode rootNode) (toNode headNode)
  sceneNode_attachObject headNode (toMovableObject ogreHead)

  -- set ambient light
  colourValue_new 0.5 0.5 0.5 1.0 >>= sceneManager_setAmbientLight smgr

  -- create a light
  l <- sceneManager_createLight1 smgr "MainLight"
  light_setPosition1 l 20 80 50

  render window root

  root_delete root

render :: RenderWindow -> Root -> IO ()
render window root = do
  windowEventUtilities_messagePump
  closed <- renderWindow_isClosed window
  when (not closed) $ do
      success <- root_renderOneFrame1 root
      when success $ render window root

