{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Todo.Postgres.TodoItem.Service (
  postgresTodoItemService,
  initPostgresPool,
  statementForNew,
  statementForFind,
) where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Either (runEitherT)
import Data.ByteString (ByteString)

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField

import Todo.Core.TodoItem.Service (
  AsTodoItemError (..),
  TodoItemError (..),
  TodoItemService (..),
 )
import Todo.Core.TodoItem.Types
import qualified Todo.Core.TodoItem.Types as TodoItem

import qualified Traction.Control as Traction
import Traction.QQ (sql)
import qualified Traction.Sql as Traction

initPostgresPool :: ByteString -> IO Traction.DbPool
initPostgresPool = Traction.newPool

instance FromField TodoItem.Id where
  fromField f b = TodoItem.Id <$> fromField f b

instance FromField TodoItem.Name where
  fromField f b = TodoItem.Name <$> fromField f b

instance ToField TodoItem.Name where
  toField (TodoItem.Name n) = toField n

instance FromField TodoItem.Status where
  fromField f mdata = do
    case mdata of
      Nothing -> returnError UnexpectedNull f ""
      Just v ->
        case v of
          "Pending" -> pure TodoPending
          "Complete" -> pure TodoComplete
          _ -> pure TodoUnknown

instance FromRow TodoItem where
  fromRow = TodoItem <$> field <*> field <*> field

postgresTodoItemService ::
  ( MonadIO m
  , MonadThrow m
  , MonadCatch m
  , AsTodoItemError e
  , MonadError e m
  ) =>
  Traction.DbPool ->
  TodoItemService m
postgresTodoItemService pool =
  TodoItemService
    { findTodoItems = pgFind pool
    , newTodoItem = pgNew pool
    , updateTodoItem = \_ -> throwing _NotImplemented "updateTodoItem"
    }

pgFind ::
  ( MonadIO m
  , AsTodoItemError e
  , MonadThrow m
  , MonadError e m
  ) =>
  Traction.DbPool ->
  m [TodoItem]
pgFind pool = do
  dbr <-
    liftIO . runEitherT $
      Traction.runDb pool statementForFind
  case dbr of
    Left e ->
      throwing _DatabaseError . Traction.renderDbError $ e
    Right tix ->
      pure tix

pgNew ::
  ( MonadIO m
  , AsTodoItemError e
  , MonadThrow m
  , MonadError e m
  ) =>
  Traction.DbPool ->
  TodoItem.Name ->
  m TodoItem
pgNew pool n = do
  dbr <-
    liftIO . runEitherT $
      Traction.runDb pool (statementForNew n)
  case dbr of
    Left e ->
      throwing _DatabaseError . Traction.renderDbError $ e
    Right ti ->
      pure ti

statementForNew ::
  Traction.MonadDb m =>
  TodoItem.Name ->
  m TodoItem
statementForNew n =
  Traction.mandatory
    [sql| 
      INSERT INTO todo
        ( key
        , name
        , status
        )
      VALUES
        ( md5(random()::text)
        , ?
        , 'Pending'
        )
      RETURNING key, name, status;
    |]
    (Traction.Only n)

statementForFind ::
  Traction.MonadDb m =>
  m [TodoItem]
statementForFind =
  Traction.query_
    [sql| SELECT key, name, status FROM todo; |]
