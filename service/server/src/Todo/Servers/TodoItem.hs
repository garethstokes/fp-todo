module Todo.Servers.TodoItem (
  server,
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (
  EitherT,
  runEitherT,
 )

import Servant
import System.IO (hPrint, stderr)

import Todo.Api (
  TodoItemApi,
  TodoItemIndex,
  mkTodoItemIndex,
 )

import Todo.Core.TodoItem.Service (
  TodoItemError (..),
  TodoItemService,
 )

import qualified Todo.Core.TodoItem.Service as TodoItemService

-- Utility
--
handleEitherT ::
  (a -> b) ->
  EitherT TodoItemError IO a ->
  Handler b
handleEitherT mkApiType a = do
  sr <- liftIO . runEitherT $ a
  case sr of
    Right x -> return (mkApiType x)
    Left err ->
      case err of
        GeneralTodoItemError txt -> do
          liftIO $ hPrint stderr txt
          throwError err500
        TodoItemNotFound _ -> do
          throwError err404
        NotImplemented txt -> do
          liftIO $ hPrint stderr txt
          throwError err500

-- Handlers
--

index ::
  TodoItemService (EitherT TodoItemError IO) ->
  Handler [TodoItemIndex]
index tis =
  handleEitherT
    mkTodoItemIndex
    $ TodoItemService.findTodoItems tis

server ::
  TodoItemService (EitherT TodoItemError IO) ->
  Server TodoItemApi
server = index
