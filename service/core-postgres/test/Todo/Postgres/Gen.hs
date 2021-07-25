module Todo.Postgres.Gen where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Todo.Core.TodoItem.Types
import qualified Todo.Core.TodoItem.Types as TodoItem

genTodoName :: MonadGen m => m TodoItem.Name
genTodoName =
  TodoItem.Name
    <$> Gen.text (Range.exponential 1 128) Gen.hexit

genTodoStatus :: MonadGen m => m TodoItem.Status
genTodoStatus =
  Gen.element [TodoPending, TodoComplete]
