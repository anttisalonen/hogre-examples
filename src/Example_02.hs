module Main
where

import Control.Monad

import Graphics.Ogre.HOgre
import Graphics.Ogre.Types

import Common

-- based on intermediate tutorial 1 from Ogre Wiki.
-- http://www.ogre3d.org/tikiwiki/Intermediate+Tutorial+1&structure=Tutorials
main :: IO ()
main = do 
  _ <- generalSetup $ \root -> do

    smgr <- root_getSceneManager root "Scene Manager"
    cam <- sceneManager_getCamera smgr "PlayerCam"

    -- set ambient light
    colourValue_with 1 1 1 1 $ sceneManager_setAmbientLight smgr

    (robotEntity, robotNode) <- addEntity smgr "robot.mesh"
    let (initx, initz) = initPos
    node_translate_NodePfloatfloatfloatNodeTransformSpace (toNode robotNode) initx 0 initz TS_WORLD

    anim <- entity_getAnimationState robotEntity "Walk"
    animationState_setLoop anim True
    animationState_setEnabled anim True

    addKnot smgr 0 (-10) 25
    addKnot smgr 550 (-10) 50
    addKnot smgr (-100) (-10) (-200)

    camera_setPosition_CameraPfloatfloatfloat cam 90 280 535
    -- camera_lookAt cam 0 0 30
    radian_with_float (-0.52359) $ camera_pitch cam
    radian_with_float (-0.26179) $ camera_yaw cam 

    window <- root_getAutoCreatedWindow root
    render window root (initState robotEntity robotNode) handler

  return ()

type OgreState = (Entity, SceneNode, (Float, Float), [(Float, Float)])

initPos :: (Float, Float)
initPos = (0, 25)

initState :: Entity -> SceneNode -> OgreState
initState e n = (e, n, initPos, cycle [(550, 50), (-100, -200), (0, 25)])

handler :: Root -> Float -> OgreState -> IO (OgreState, Bool)
handler _ delta (ent, node, (locx, locz), wl) = do
  let (wx, wz) = head wl
  dist <- 
    vector3_with_floatfloatfloat locx 0 locz $ \locv ->
       vector3_with_floatfloatfloat wx 0 wz $ vector3_distance locv
  let wl' = if dist <= 30
              then tail wl
              else wl
      (tgtx, tgtz) = head wl'
      ang = atan2 (locz - tgtz) (locx - tgtx)
      locx' = locx - 35 * delta * cos ang
      locz' = locz - 35 * delta * sin ang
  when (dist > 150) $
    vector3_with_floatfloatfloat tgtx 0 tgtz $ \v1 -> 
      vector3_with_floatfloatfloat 1 0 0 $ \v -> 
       sceneNode_lookAt node v1 TS_WORLD v
  node_setPosition (toNode node) locx' 0 locz'
  anim <- entity_getAnimationState ent "Walk"
  animationState_addTime anim delta
  return ((ent, node, (locx', locz'), wl'), True)

addKnot :: SceneManager -> Float -> Float -> Float -> IO ()
addKnot smgr v1 v2 v3 = do
  (_, knot) <- addEntity smgr "knot.mesh"
  node_translate_NodePfloatfloatfloatNodeTransformSpace (toNode knot) v1 v2 v3 TS_WORLD
  node_setScale (toNode knot) 0.1 0.1 0.1

