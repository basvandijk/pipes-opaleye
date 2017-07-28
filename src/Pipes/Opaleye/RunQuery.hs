{-# language ScopedTypeVariables, FlexibleContexts #-}

module Pipes.Opaleye.RunQuery ( query ) where

import Control.Exception (handle, throwTo, SomeException)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Opaleye (QueryRunner, Query, runQueryFold)
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple (Connection)
import Pipes (Producer, yield)
import Pipes.Safe (SafeT, bracket)
import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread, myThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Data.Function (fix)
import Data.Foldable (for_)

query :: forall columns haskells m
       . (Default QueryRunner columns haskells, MonadIO m, MonadMask m)
      => Connection -> Query columns -> Producer haskells (SafeT m) ()
query conn query = do
    bracket (liftIO fork) (liftIO . killThread . snd) $ \(mv, _tid) ->
      fix $ \loop -> do
        mbA <- liftIO $ takeMVar mv
        for_ mbA $ \a -> yield a *> loop
  where
    fork :: IO (MVar (Maybe haskells), ThreadId)
    fork = do
       myTid <- myThreadId
       let throwAllToMe :: SomeException -> IO ()
           throwAllToMe = throwTo myTid
       mv <- newEmptyMVar
       tid <- forkIOWithUnmask $ \unmask -> handle throwAllToMe $ unmask $ do
                runQueryFold conn query () $ \() a -> putMVar mv (Just a)
                putMVar mv Nothing
       pure (mv, tid)
