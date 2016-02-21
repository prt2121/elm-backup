module Main where

import Console exposing (IO, (>>>), (>>=), forever, getLine, pure, exit, putStr, putStrLn)
import Task

echo : IO ()
echo = forever (getLine >>= putStrLn)

loop : IO ()
loop = putStr "λ> " >>>
       getLine >>= \s ->
       if s == "exit"
       then pure ()
       else putStrLn s >>> loop

hello : IO ()
hello = loop >>>
        exit 0

port runner : Signal (Task.Task x ())
port runner = Console.run hello
