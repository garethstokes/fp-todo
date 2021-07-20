{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Todo.Server 
  ( app
  , exceptionResponse
  ) 
where

import Control.Exception (SomeException)
import Control.Monad.Trans.Either (EitherT)
import Data.Text (Text)
import Data.Aeson (encode)
import Network.HTTP.Types 
  ( internalServerError500
  , hContentType
  )
import Network.Wai (Response, responseLBS)
import Servant

import Todo.Api (TodoItemApi)
import Todo.Core.TodoItem.Service 
  ( TodoItemService
  , TodoItemError
  )

import qualified Todo.Servers.TodoItem as TodoItem

-- HANDLERS
--

healthz
  :: Handler Text
healthz =
  pure "OK."

-- APP / SERVER
--

type Api = 
  ("healthz" :> Get '[PlainText] Text)
  :<|> TodoItemApi

app 
  :: TodoItemService (EitherT TodoItemError IO) 
  -> Application
app tis =
  serve
    (Proxy :: Proxy Api)
    $ healthz 
        :<|> TodoItem.server tis

exceptionResponse :: SomeException -> Response
exceptionResponse e =
  responseLBS 
    internalServerError500 
    [(hContentType, "application/json; charset=utf-8")] 
    $ encode (show e)
