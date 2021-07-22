module Main where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Handler.Warp (
  defaultSettings,
  runSettings,
  setLogger,
  setOnExceptionResponse,
  setPort,
 )
import Network.Wai.Logger (withStdoutLogger)
import System.Exit (exitFailure)
import System.IO (
  BufferMode (NoBuffering),
  hPrint,
  hSetBuffering,
  stderr,
  stdout,
 )

import Todo.Postgres.TodoItem.Migration (
  postgresRunMigrations,
 )
import Todo.Postgres.TodoItem.Service (
  initPostgresPool,
  postgresTodoItemService,
 )
import Todo.Util.Logger (logger)
import Todo.Util.ServiceEnvironment

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

  liftIO . putStrLn $ "todo-server: initiating database connection"
  pool <- initPostgresPool (connectionString cse)

  liftIO . putStrLn $ "todo-server: running database migrations"
  mr <- postgresRunMigrations pool
  case mr of
    Left e -> do
      hPrint stderr e
      exitFailure
    Right mx -> do
      print mx
      pure ()

  putStrLn $
    "todo-server: starting a web server on port "
      <> show (servicePort cse)

  withStdoutLogger $ \aplogger -> do
    let settings =
          setPort (servicePort cse) $
            setLogger (logger aplogger) $
              setOnExceptionResponse
                TodoServer.exceptionResponse
                defaultSettings

    runSettings
      settings
      $ TodoServer.app $
        postgresTodoItemService pool
    putStrLn "todo-server: stopping web server"
