{-# LANGUAGE OverloadedStrings #-}

module Todo.Servers.Test.TodoApp where

import Control.Lens (view)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant.Client
import Test.Hspec
import Test.Hspec.Wai
import Todo.Api (TodoItemApi)
import Todo.Api.Types.TodoItem (mkTodoItemIndex)
import Todo.Core.TodoItem.Types
import qualified Todo.Core.TodoItem.Types as TodoItem
import Todo.Memory.TodoItem.Service (
  memoryTodoItemService,
 )
import qualified Todo.Server as Todo

withTodoApp :: (Warp.Port -> IO ()) -> IO ()
withTodoApp action = do
  let ctx = Map.fromList $ zip (map (view tiId) todos) todos
  -- testWithApplication makes sure the action is executed after the server has
  -- started and is being properly shutdown.
  Warp.testWithApplication
    (pure . Todo.app $ memoryTodoItemService ctx)
    action

todos :: [TodoItem]
todos =
  [ TodoItem (TodoItem.Id "todo-001") (TodoItem.Name "todo 1") TodoPending
  , TodoItem (TodoItem.Id "todo-002") (TodoItem.Name "todo 2") TodoPending
  , TodoItem (TodoItem.Id "todo-003") (TodoItem.Name "todo 3") TodoPending
  ]

todoAppSpec :: Spec
todoAppSpec =
  -- `around` will start a web server to run the tests against
  around withTodoApp $ do
    -- let (getTodos :<|> postTodo) =
    --    client (Proxy :: Proxy TodoItemApi)
    let getTodos = client (Proxy :: Proxy TodoItemApi)
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

    describe "GET /todos" $ do
      it "should return 3 todo items" $ \port -> do
        result <- runClientM getTodos (clientEnv port)
        result `shouldBe` (Right . mkTodoItemIndex $ todos)
