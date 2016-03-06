module Printer (..) where

import Types exposing (..)

-- return a string representation of the input


pr_str : MalVal -> String
pr_str v =
  case v of
    MalString str ->
      str

    MalSymbol name ->
      name

    MalTrue ->
      "true"

    MalFalse ->
      "false"

    Nil ->
      "nil"

    otherwise ->
      "otherwise"


pr_list : String -> List MalVal -> String
pr_list sep ls =
  case ls of
    [] ->
      ""

    x :: [] ->
      (pr_str x)

    x :: xs ->
      (pr_str x) ++ sep ++ (pr_list sep xs)
