{-# LANGUAGE OverloadedStrings #-}

module Todo.Util.ServiceEnvironment (
  ServiceEnvironment (..),
  ServiceEnvironmentError (..),
  serviceEnvironment,
) where

import Control.Monad.Except (withExceptT)
import Nest (NestError, numeric, runT, string, withDefault)

newtype ServiceEnvironmentError
  = ServiceEnvironmentError NestError
  deriving (Show)

data ServiceEnvironment = ServiceEnvironment
  { servicePort :: Int
  , connectionString :: ByteString
  }

serviceEnvironment ::
  MonadIO m =>
  ExceptT ServiceEnvironmentError m ServiceEnvironment
serviceEnvironment =
  withExceptT ServiceEnvironmentError $
    ServiceEnvironment
      <$> runT (withDefault (numeric "PORT") 3031)
      <*> runT (withDefault (string "DB") "host=localhost dbname=todo")
