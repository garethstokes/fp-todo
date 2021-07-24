{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Todo.Core.TodoItem.Types where

import Control.Lens.TH (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype Id
  = Id Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

newtype Name
  = Name Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Status
  = TodoPending
  | TodoComplete
  | TodoUnknown
  deriving (Eq, Ord, Show, Generic)

data TodoItem = TodoItem
  { _tiId :: Id
  , _tiName :: Name
  , _tiStatus :: Status
  }
  deriving (Eq, Ord, Show, Generic)

makeLenses ''TodoItem

renderId (TodoItem (Id todoId) _ _) = todoId
renderName (TodoItem _ (Name todoName) _) = todoName
