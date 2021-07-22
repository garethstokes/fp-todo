{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Todo.Postgres.TodoItem.Migration where

import Control.Monad.Trans.Either (runEitherT)
import Database.PostgreSQL.Simple (Query)
import Traction.Control (DbPool)
import Traction.Migration (Migration (Migration))
import Traction.QQ (sql)

import qualified Traction.Control as Traction
import qualified Traction.Migration as Traction

postgresRunMigrations ::
  DbPool ->
  IO (Either Traction.DbError [Migration])
postgresRunMigrations pool =
  runEitherT $
    Traction.runDb pool $
      Traction.migrate migrations

migrations :: [Migration]
migrations =
  [ Migration
      "create-todo-table"
      [sql|
      CREATE TABLE todo
        ( id SERIAL PRIMARY KEY
        , key TEXT UNIQUE NOT NULL
        , name TEXT NOT NULL
        , status TEXT NOT NULL
        , created TIMESTAMPTZ NOT NULL DEFAULT now()
        , updated TIMESTAMPTZ NOT NULL DEFAULT now()
        )
    |]
  ]
