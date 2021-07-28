{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Todo.Server (
  app,
  exceptionResponse,
) where

import Control.Monad.Trans.Either (EitherT)
import Data.Aeson (encode)
import Network.HTTP.Types (
  hContentType,
  internalServerError500,
 )
import Network.Wai (Response, responseLBS)
import Servant

import Todo.Api (TodoItemApi)
import Todo.Core.TodoItem.Service (
  TodoItemError,
  TodoItemService,
 )

import qualified Todo.Servers.TodoItem as TodoItem

-- HANDLERS
--

healthz ::
  Handler Text
healthz =
  pure "OK."

-- APP / SERVER
--

type Api =
  ("healthz" :> Get '[PlainText] Text)
    :<|> TodoItemApi

app ::
  TodoItemService (EitherT TodoItemError IO) ->
  Application
app tis =
  serve
    (Proxy :: Proxy Api)
    $ healthz
      :<|> TodoItem.server tis

exceptionResponse ::
  SomeException ->
  Response
exceptionResponse e =
  responseLBS
    internalServerError500
    [(hContentType, "application/json; charset=utf-8")]
    $ encode (show e :: Text)
