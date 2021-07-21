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
  = DatabaseError Text
  | TodoItemNotFound TodoItem.Id
  | NotImplemented Text
  deriving (Show)

makeClassyPrisms ''TodoItemError

data TodoItemService m = TodoItemService
  { findTodoItems :: m [TodoItem]
  , newTodoItem :: TodoItem.Name -> m TodoItem
  , updateTodoItem :: TodoItem -> m TodoItem
  }
