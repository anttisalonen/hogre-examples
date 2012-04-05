module Common
where

import System.Exit
import Control.Monad

import Graphics.Ogre.HOgre
import Graphics.Ogre.Types

-- based on basic tutorial 6 from Ogre Wiki.
-- http://www.ogre3d.org/tikiwiki/Basic+Tutorial+6&structure=Tutorials
generalSetup :: (Root -> IO a) -> IO a
generalSetup fun =
  -- construct Ogre::Root
  root_with "plugins.cfg" "ogre.cfg" "Ogre.log" $ \root -> do

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
    smgr <- root_createSceneManager_RootPcharPcharP root "DefaultSceneManager" "Scene Manager"

    -- create and position the camera
    cam <- sceneManager_createCamera smgr "PlayerCam"
    frustum_setNearClipDistance (toFrustum cam) 5

    -- create one viewport, entire window
    vp <- renderTarget_addViewport (toRenderTarget window) cam 0 0 0 1 1
    colourValue_with 0 0 0 1 $ viewport_setBackgroundColour vp

    -- Alter the camera aspect ratio to match the viewport
    vpw <- viewport_getActualWidth vp
    vph <- viewport_getActualHeight vp
    frustum_setAspectRatio (toFrustum cam) (fromIntegral vpw / fromIntegral vph)

    fun root

render :: RenderWindow -> Root -> a -> (Root -> Float -> a -> IO (a, Bool)) -> IO a
render window root value fun = do
    timer <- root_getTimer root
    time <- timer_getMicroseconds timer
    render' time window root value fun

render' :: Int -> RenderWindow -> Root -> a -> (Root -> Float -> a -> IO (a, Bool)) -> IO a
render' time window root value fun = 
  do windowEventUtilities_messagePump
     closed <- renderWindow_isClosed window
     if closed
       then return value
       else do
         success <- root_renderOneFrame_RootP root
         timer <- root_getTimer root
         time' <- timer_getMicroseconds timer
         let delta = (fromIntegral (time' - time)) / 1000000
         (value', cont) <- fun root delta value
         if success && cont
           then render' time' window root value' fun
           else return value'

nullHandler :: Root -> Float -> () -> IO ((), Bool)
nullHandler _ _ _ = return ((), True)

addEntity :: SceneManager -> String -> IO (Entity, SceneNode)
addEntity smgr mesh = do
  ent <- sceneManager_createEntity_SceneManagerPcharP smgr mesh
  rootNode <- sceneManager_getRootSceneNode smgr
  node <- sceneManager_createSceneNode_SceneManagerP smgr
  node_addChild (toNode rootNode) (toNode node)
  sceneNode_attachObject node (toMovableObject ent)
  return (ent, node)

