{-# LANGUAGE ForeignFunctionInterface #-}
module Main
where

import CTypes
import CString


foreign import ccall "ogre.h init" c_init :: IO ()
foreign import ccall "ogre.h add_object" c_add_object :: CString -> CString -> IO ()
foreign import ccall "ogre.h set_object_position" c_set_object_position :: CString -> CFloat -> CFloat -> CFloat -> IO ()
foreign import ccall "ogre.h cleanup" c_cleanup :: IO ()
foreign import ccall "ogre.h render" c_render :: IO ()

render_loop :: IO ()
render_loop = do
    c_render
    render_loop

main :: IO ()
main = do
    c_init
    objname <- newCString "obj1"
    withCString "robot.mesh" (c_add_object objname)
    c_set_object_position objname 5.0 20.0 10.0
    render_loop
