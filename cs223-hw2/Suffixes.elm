module Suffix where

suffixes : List a -> List (List a)
suffixes ls =
  case ls of
    []              -> [[]]
    ((x::xs) as ls) -> ls :: suffixes xs
