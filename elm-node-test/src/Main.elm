-- module Main where
--
-- import Console
-- import Task exposing (Task)
--
-- port main : Task String ()
-- port main =
--   Console.log "hello, world"

module Main where

import Task exposing (Task,andThen,onError)
import Console
import File

port main : Task x ()
port main =
    File.read "elm-stuff/exact-dependencies.json"
        `andThen` Console.log
        `onError` Console.fatal
