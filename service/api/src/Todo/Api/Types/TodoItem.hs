{-# LANGUAGE TemplateHaskell #-}

module Todo.Api.Types.TodoItem (
  TodoItemIndex,
  mkTodoItemIndex,
) where

import Control.Lens (view)
import Data.Aeson (ToJSON)
import Data.Aeson.TH
import Todo.Core.TodoItem.Types (TodoItem, tiId, tiName)
import qualified Todo.Core.TodoItem.Types as TodoItem

data TodoItemIndex = TodoItemIndex
  { _tliId :: TodoItem.Id
  , _tliName :: TodoItem.Name
  }
  deriving (Eq, Show)

$( deriveJSON
    defaultOptions
      { fieldLabelModifier = drop 4
      , omitNothingFields = True
      }
    ''TodoItemIndex
 )

mkTodoItemIndex :: [TodoItem] -> [TodoItemIndex]
mkTodoItemIndex = map toIndex
 where
  toIndex ti =
    TodoItemIndex
      (view tiId ti)
      (view tiName ti)
