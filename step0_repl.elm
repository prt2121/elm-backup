module Main where

import Console exposing (IO, (>>>), (>>=), forever, getLine, pure, exit, putStr, putStrLn)
import Task

-- read
mal_read : String -> String
mal_read str = str

-- eval
eval : String -> String -> String
eval ast env = ast

-- print
mal_print : String -> String
mal_print exp = exp

-- repl
rep : String -> String
rep line = mal_print <| eval (mal_read line) ""

echo : IO ()
echo = forever (getLine >>= putStrLn)

loop : IO ()
loop = putStr "user> " >>>
       getLine >>= \s ->
         if s == ""
         then pure ()
         else (putStrLn <| rep s) >>> loop
         -- else putStrLn s >>> loop

repl_loop : IO ()
repl_loop = loop >>>
            exit 0

port runner : Signal (Task.Task x ())
port runner = Console.run repl_loop 
