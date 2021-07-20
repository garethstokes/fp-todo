{-# LANGUAGE OverloadedStrings #-}
module Todo.Util.ServiceEnvironment 
  ( ServiceEnvironment (..)
  , ServiceEnvironmentError (..)
  , serviceEnvironment
  ) where

import Data.ByteString (ByteString)
import Nest (NestError, string, numeric, runT, withDefault)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (ExceptT, withExceptT)

newtype ServiceEnvironmentError 
  = ServiceEnvironmentError NestError
  deriving Show

data ServiceEnvironment =
  ServiceEnvironment 
    { servicePort :: Int
    , connectionString :: ByteString
    }

serviceEnvironment
  :: MonadIO m
  => ExceptT ServiceEnvironmentError m ServiceEnvironment
serviceEnvironment =
  withExceptT ServiceEnvironmentError $
    ServiceEnvironment 
      <$> runT (withDefault (numeric "PORT") 3031) 
      <*> runT (withDefault (string "DB") "host=localhost dbname=consuela")
