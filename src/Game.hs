{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances, FunctionalDependencies #-}
module Game(Game, 
        Game.init, cleanup, render, renew, update, process, input,
        run, runThreaded, runThreadedNonblocking)
where

import Data.List(foldl')
import Control.Monad (forever)
import Control.Concurrent

class Game a b c | a -> b c where
    init :: a -> IO ()
    init _ = return ()
    cleanup :: a -> IO ()
    cleanup _ = return ()
    render :: a -> IO ()
    renew :: a -> a
    update :: a -> b -> a
    process :: a -> [b]
    input :: a -> IO (Either c [b])

-- Everything runs in one thread. Input should block.
run :: (Game a b c) => a -> IO c
run g = Game.init g >> run' g

run' :: (Game a b c) => a -> IO c
run' g = do
    render g
    i <- handle g
    case i of
      Left  end -> cleanup g >> return end
      Right g'  -> run' g'

-- Use this when input blocks.
-- Returns either c or new state.
handle :: (Game a b c) => a -> IO (Either c a)
handle g = do
    i <- input g
    case i of
      Left  end -> return $ Left end
      Right is  -> return $ Right $ step g is

-- Single update step.
step :: (Game a b c) => a -> [b] -> a
step g is = let as = process g
                g' = updateAll g (as ++ is)
            in renew g'

updateAll :: (Game a b c) => a -> [b] -> a
updateAll = foldl' update

-- Use this when stepping without blocking.
handleThreaded :: (Game a b c) => MVar a -> IO ()
handleThreaded mv = modifyMVar_ mv $ \g -> return (step g [])

renderThreaded :: (Game a b c) => MVar a -> IO ()
renderThreaded mg = withMVar mg render 

-- Main thread is reserved for blocking input.
-- Two other threads will be started for rendering and for updating.
runThreaded :: (Game a b c) => a -> Int -> Int -> IO c
runThreaded g renderinterval handleinterval = do
   Game.init g
   mv <- newMVar g
   let ri = renderinterval * 1000
   let si = handleinterval * 1000
   rtid <- forkIO (forever (renderThreaded mv >> threadDelay ri))
   haid <- forkIO (forever (handleThreaded mv >> threadDelay si))
   loopThreaded mv [rtid, haid]

loopThreaded :: (Game a b c) => MVar a -> [ThreadId] -> IO c
loopThreaded = loopThreadedGen updateAll (return ())

loopThreadedNonblocking :: (Game a b c) => Int -> MVar a -> [ThreadId] -> IO c
loopThreadedNonblocking si = loopThreadedGen step (threadDelay si)

loopThreadedGen :: (Game a b c) => (a -> [b] -> a) -> IO () -> MVar a -> [ThreadId] -> IO c
loopThreadedGen updfunc waitfunc mv tids = do
  n <- modifyMVar mv (\g -> do
    i <- input g
    case i of
      Left  e -> fullCleanup tids (cleanup g) >> return (g, Just e)
      Right n -> do
        let g' = updfunc g n
        return (g', Nothing))
  case n of
    Just e  -> return e
    Nothing -> waitfunc >> loopThreadedGen updfunc waitfunc mv tids

fullCleanup :: [ThreadId] -> IO () -> IO ()
fullCleanup tids cf = mapM_ killThread tids >> cf

-- Main thread polls nonblocking input and updates the world.
-- Another thread is started for rendering.
runThreadedNonblocking :: (Game a b c) => a -> Int -> Int -> IO c
runThreadedNonblocking g renderinterval handleinterval = do
   Game.init g
   mv <- newMVar g
   let ri = renderinterval * 1000
   let si = handleinterval * 1000
   rtid <- forkIO (forever (renderThreaded mv >> threadDelay ri))
   loopThreadedNonblocking si mv [rtid]

