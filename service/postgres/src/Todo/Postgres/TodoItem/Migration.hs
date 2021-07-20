{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Todo.Postgres.TodoItem.Migration where

import Traction.Migration (Migration(Migration))
import Traction.QQ (sql)
import Database.PostgreSQL.Simple (Query)

migrations :: [Migration]
migrations = 
  [ Migration "create-todo-table" [sql|
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
