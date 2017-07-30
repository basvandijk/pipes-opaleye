{-# language ScopedTypeVariables, FlexibleContexts #-}

module Pipes.Opaleye.RunQuery ( query ) where

import Control.Exception (handle, throwTo, fromException, SomeException, AsyncException(ThreadKilled))
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Opaleye (QueryRunner, Query, runQueryFold)
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple (Connection)
import Pipes (ListT(Select), yield)
import Pipes.Safe (SafeT, bracket)
import Control.Concurrent (ThreadId, forkIOWithUnmask, killThread, myThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Data.Function (fix)
import Data.Foldable (for_)

query :: forall columns haskells m
       . (Default QueryRunner columns haskells, MonadIO m, MonadMask m)
      => Connection -> Query columns -> ListT (SafeT m) haskells
query conn query = Select $ do
    bracket (liftIO fork) (liftIO . killThread . snd) $ \(mv, _tid) ->
      fix $ \loop -> do
        mbA <- liftIO $ takeMVar mv
        for_ mbA $ \a -> yield a *> loop
  where
    fork :: IO (MVar (Maybe haskells), ThreadId)
    fork = do
       myTid <- myThreadId
       let throwToMe :: SomeException -> IO ()
           throwToMe ex = case fromException ex of
                            Just ThreadKilled -> pure ()
                            _ -> throwTo myTid ex
       mv <- newEmptyMVar
       tid <- forkIOWithUnmask $ \unmask -> handle throwToMe $ unmask $ do
                runQueryFold conn query () $ \() a -> putMVar mv (Just a)
                putMVar mv Nothing
       pure (mv, tid)
