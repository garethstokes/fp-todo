{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Todo.Api
  ( TodoItemApi
  , module Todo.Api.Types.TodoItem
  )
where

import Servant.API ((:>), Get, JSON)
import Todo.Api.Types.TodoItem

type TodoItemApi = 
  "todos" :> Get '[JSON] [TodoItemIndex]
