module Todo.Postgres.Statement.Test where

import Control.Lens
import Hedgehog

import Control.Monad.Morph (hoist, lift)
import Control.Monad.Trans.Either (EitherT)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Todo.Core.TodoItem.Types (
  Status (TodoPending),
  tiName,
  tiStatus,
 )
import Todo.Postgres.Gen (genTodoName, genTodoStatus)
import Todo.Postgres.TodoItem.Service (
  statementForFind,
  statementForNew,
 )
import Traction.Control (Db, DbPool, runDb)

import Todo.Core.TodoItem.Service
import qualified Todo.Core.TodoItem.Service as TodoItem

tests ::
  DbPool ->
  TestTree
tests p =
  testGroup
    "TodoItem"
    [ newTodo p
    , findTodos p
    ]

runService ::
  EitherT TodoItemError IO a ->
  PropertyT IO a
runService = evalExceptT . hoist lift

runQuery :: DbPool -> Db a -> PropertyT IO a
runQuery p a = evalExceptT . hoist lift $ runDb p a

newTodo :: DbPool -> TestTree
newTodo p =
  testProperty "newTodo" $
    withTests 5 . property $ do
      n <- forAll genTodoName
      s <- forAll genTodoStatus
      ti <- runQuery p $ statementForNew n s

      view tiName ti === n
      view tiStatus ti === s

findTodos :: DbPool -> TestTree
findTodos p =
  testProperty "findTodoItems" $
    withTests 5 . property $ do
      n1 <- forAll genTodoName
      n2 <- forAll genTodoName
      n3 <- forAll genTodoName

      tix <- runQuery p $ do
        statementForNew n1 TodoPending
        statementForNew n2 TodoPending
        statementForNew n3 TodoPending
        statementForFind

      length tix === 3
