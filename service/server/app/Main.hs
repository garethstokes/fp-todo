module Main where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Logger (withStdoutLogger)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    runSettings,
    setLogger,
    setOnExceptionResponse,
    setPort,
  )
import System.Exit (exitFailure)
import System.IO 
  ( BufferMode(NoBuffering)
  , hPrint
  , stderr
  , stdout
  , hSetBuffering
  )

import Todo.Util.ServiceEnvironment
import Todo.Util.Logger (logger)
import Todo.Postgres.TodoItem.Service (postgresTodoItemService)

import qualified Todo.Server as TodoServer

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  liftIO . putStrLn $ "todo-server: starting"
  liftIO . putStrLn $ "todo-server: processing environment"

  cse <- do
    res <- runExceptT serviceEnvironment
    case res of
      Left e -> do
        hPrint stderr e
        exitFailure
      Right y ->
        pure y

  putStrLn
    $ "todo-server: starting a web server on port "
    <> show (servicePort cse)

  withStdoutLogger $ \aplogger -> do
    let
      settings = setPort (servicePort cse)
                  $ setLogger (logger aplogger)
                  $ setOnExceptionResponse 
                      TodoServer.exceptionResponse
                      defaultSettings

    runSettings 
      settings 
      $ TodoServer.app postgresTodoItemService
    putStrLn "todo-server: stopping web server"
