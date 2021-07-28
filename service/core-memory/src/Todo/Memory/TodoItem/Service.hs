{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Todo.Memory.TodoItem.Service (
  memoryTodoItemService,
) where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Except (MonadError)
import qualified Relude.Extra.Map as Map
import Todo.Core.TodoItem.Service (
  AsTodoItemError (..),
  TodoItemService (..),
 )
import Todo.Core.TodoItem.Types
import qualified Todo.Core.TodoItem.Types as TodoItem

type MemoryContext = Map TodoItem.Id TodoItem

initMemoryTodoService :: Map TodoItem.Id TodoItem
initMemoryTodoService = mempty

memoryTodoItemService ::
  ( MonadIO m
  , MonadThrow m
  , MonadCatch m
  , AsTodoItemError e
  , MonadError e m
  ) =>
  MemoryContext ->
  TodoItemService m
memoryTodoItemService ctx =
  TodoItemService
    { findTodoItems = memFind ctx
    , newTodoItem = \_ _ -> throwing _NotImplemented "newTodoItem"
    , updateTodoItem = \_ -> throwing _NotImplemented "updateTodoItem"
    }

memFind ::
  ( MonadIO m
  , AsTodoItemError e
  , MonadThrow m
  , MonadError e m
  ) =>
  MemoryContext ->
  m [TodoItem]
memFind = pure . Map.elems
