module Types (..) where

import Dict exposing (..)


-- TODO: MalAtom, Func, MalFunc


type MalVal
  = Nil
  | MalFalse
  | MalTrue
  | MalNumber Int
  | MalString String
  | MalSymbol String
  | MalList (List MalVal) MalVal
  | MalVector (List MalVal) MalVal
  | MalHashMap (Dict String MalVal) MalVal


type MalError
  = StringError String
  | MalValError MalVal

-- type alias IOThrows =
