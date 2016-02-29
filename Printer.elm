module Printer (..) where

import Types exposing (..)

-- return a string representation of the input
pr_str : MalVal -> String
pr_str v =
  case v of
    (MalString str) -> str
    (MalSymbol name) -> name
    MalTrue -> "true"
    MalFalse -> "false"
    Nil -> "nil"
    otherwise -> "otherwise"
