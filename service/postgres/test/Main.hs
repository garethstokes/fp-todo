{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Control.Monad.Trans.Either (runEitherT)
import Nest (string, force, withDefault)
import System.Exit (exitFailure)
import System.IO (hPrint, stderr, stdout)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Traction.Control (DbPool)
import Database.PostgreSQL.Simple (Query)

import qualified Todo.Postgres.TodoItem.Migration as TodoItem
import qualified Todo.Postgres.Test as Postgres
import qualified Traction.Sql as Traction
import qualified Traction.Control as Traction
import qualified Traction.Migration as Traction

tests 
  :: DbPool 
  -> TestTree
tests pool =
  testGroup
    "Postgres"
    [ Postgres.tests pool
    ]

main ::
  IO ()
main = do
  -- initiating environment
  cs <- force 
          $ withDefault 
              (string "DB") 
              "host=localhost dbname=todo_tests"

  -- initiating database connection
  pool <- Traction.newPool cs
  poolWithRollback <- Traction.newRollbackPool cs

  -- running database migrations
  putStrLn "running database migrations"
  mr <- runEitherT 
    $ Traction.runDb pool 
    $ Traction.migrate TodoItem.migrations
  case mr of
    Left e -> do
      hPrint stderr e
      exitFailure
    Right mx -> do
      print mx
      pure ()

  -- running tests
  defaultMain $ tests poolWithRollback


runTraction
  :: DbPool
  -> Query
  -> IO ()
runTraction pool a = do
  rr <- runEitherT
          $ Traction.runDb pool
          $ Traction.execute_
          $ a
  case rr of
    Left e -> do
      hPrint stderr e
      exitFailure
    Right _ -> do
      pure ()
