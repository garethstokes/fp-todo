{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.Core.TodoItem.Types where

import Control.Lens.TH (makeLenses)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Id = 
  Id Text 
  deriving (Eq, Ord, Show, Generic)

newtype Name = 
  Name Text 
  deriving (Eq, Ord, Show, Generic)

data Status
  = TodoPending
  | TodoComplete
  | TodoUnknown
  deriving (Eq, Ord, Show, Generic)

data TodoItem = 
  TodoItem 
    { _tiId :: Id
    , _tiName :: Name
    , _tiStatus :: Status
    }
    deriving (Eq, Ord, Show, Generic)

makeLenses ''TodoItem

renderId (TodoItem (Id todoId) _ _) = todoId
renderName (TodoItem _ (Name todoName) _ ) = todoName
