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

instance ToField TodoItem.Status where
  toField tis = toField (todoStatus :: ByteString)
   where
    todoStatus =
      case tis of
        TodoPending -> "Pending"
        TodoComplete -> "Complete"
        _ -> "Unknown"

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

execStatement p s = do
  dbr <-
    liftIO . runEitherT $
      Traction.runDb p s
  case dbr of
    Left e ->
      throwing _GeneralTodoItemError . Traction.renderDbError $ e
    Right tix ->
      pure tix

pgFind ::
  ( MonadIO m
  , AsTodoItemError e
  , MonadThrow m
  , MonadError e m
  ) =>
  Traction.DbPool ->
  m [TodoItem]
pgFind pool = execStatement pool statementForFind

pgNew ::
  ( MonadIO m
  , AsTodoItemError e
  , MonadThrow m
  , MonadError e m
  ) =>
  Traction.DbPool ->
  TodoItem.Name ->
  TodoItem.Status ->
  m TodoItem
pgNew pool n s = execStatement pool $ statementForNew n s

statementForNew ::
  Traction.MonadDb m =>
  TodoItem.Name ->
  TodoItem.Status ->
  m TodoItem
statementForNew n s =
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
        , ?
        )
      RETURNING key, name, status;
    |]
    (n, s)

statementForFind ::
  Traction.MonadDb m =>
  m [TodoItem]
statementForFind =
  Traction.query_
    [sql| SELECT key, name, status FROM todo; |]
