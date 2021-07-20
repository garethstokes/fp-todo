{-# LANGUAGE TemplateHaskell #-}

module Todo.Api.Types.TodoItem
  ( TodoItemIndex
  , mkTodoItemIndex
  )
where

import Data.Aeson (ToJSON)
import Data.Aeson.TH
import Data.Text (Text)
import Todo.Core.TodoItem.Types (TodoItem, renderId, renderName)

data TodoItemIndex = TodoItemIndex
  { _tliId    :: Text
  , _tliName  :: Text
  }
  deriving (Eq, Show)

$(deriveJSON 
    defaultOptions 
    { fieldLabelModifier = drop 4, omitNothingFields = True } 
    ''TodoItemIndex
 )

mkTodoItemIndex :: [TodoItem] -> [TodoItemIndex]
mkTodoItemIndex = map toIndex
  where toIndex ti = 
          TodoItemIndex
            (renderId ti)
            (renderName ti)
    
