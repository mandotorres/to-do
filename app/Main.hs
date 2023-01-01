module Main where

import App (getTodoItems)
import System.Console.ANSI (clearScreen)
import Utils (getToDos, loop)

main :: IO ()
main = do
  clearScreen
  putStrLn "Haskell To Do Application"
  ests <- getToDos -- either string/todos
  loop $ getTodoItems ests