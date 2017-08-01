{-# language ScopedTypeVariables, FlexibleContexts #-}

module Pipes.Opaleye.RunQuery ( query ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Opaleye (QueryRunner, Query)
import Opaleye.RunQuery (declareCursor, closeCursor, foldForward)
import Data.Profunctor.Product.Default (Default)
import Database.PostgreSQL.Simple (Connection)
import Pipes (ListT(Select), yield)
import Pipes.Safe (SafeT, bracket)
import Data.Function (fix)

query :: forall columns haskells m
       . ( Default QueryRunner columns haskells
         , MonadIO m
         , MonadMask m
         )
      => Connection
      -> Int -- ^ chunk size
      -> Query columns
      -> ListT (SafeT m) haskells
query conn chunkSize q = Select $
    bracket (liftIO $ declareCursor conn q)
            (liftIO . closeCursor) $ \cursor ->
      fix $ \loop -> do
        r <- liftIO $ foldForward cursor chunkSize
               (\f haskells -> pure $ (yield haskells *>) . f) id
        case r of
          Left  f -> f (pure ())
          Right f -> f loop
