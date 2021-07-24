{-# LANGUAGE TemplateHaskell #-}

module Todo.Core.TodoItem.Service (
  TodoItemError (..),
  TodoItemService (..),
  AsTodoItemError (..),
) where

import Control.Lens.TH (makeClassyPrisms)
import Data.Text (Text)
import Todo.Core.TodoItem.Types (TodoItem)
import qualified Todo.Core.TodoItem.Types as TodoItem

data TodoItemError
  = TodoItemNotFound TodoItem.Id
  | GeneralTodoItemError Text
  | NotImplemented Text
  deriving (Show)

makeClassyPrisms ''TodoItemError

data TodoItemService m = TodoItemService
  { findTodoItems :: m [TodoItem]
  , newTodoItem :: TodoItem.Name -> TodoItem.Status -> m TodoItem
  , updateTodoItem :: TodoItem -> m TodoItem
  }
