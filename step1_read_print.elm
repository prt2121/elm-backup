module Main (..) where

import Console exposing (IO, (>>>), (>>=), forever, getLine, pure, exit, putStr, putStrLn)
import Task


-- read


mal_read : String -> String
mal_read str =
  str



-- eval


eval : String -> String -> String
eval ast env =
  ast



-- print


mal_print : String -> String
mal_print exp =
  exp



-- repl


rep : String -> String
rep line =
  eval (mal_read line) ""
    |> mal_print


repl_loop : IO ()
repl_loop =
  putStr "user> "
    >>> getLine
    >>= \s ->
          if s == "" then
            pure ()
          else
            (rep s |> putStrLn) >>> repl_loop


port runner : Signal (Task.Task x ())
port runner =
  repl_loop >>> exit 0
    |> Console.run
