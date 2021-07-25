module Main where

import Test.Hspec
import Todo.Servers.Test.TodoApp

main :: IO ()
main = hspec todoAppSpec
