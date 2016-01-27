module FP where

import List ((::))  -- TODO: modify imports if you'd like
import List
import Result (..)

digitsOfInt : Int -> List Int
digitsOfInt n =
  if n < 0 then
    []
  else if n < 10 then
    [n]
  else
    digitsOfInt (n // 10) ++ [n `rem` 10]

additivePersistence : Int -> Int
additivePersistence n =
  -- TODO
  0

digitalRoot : Int -> Int
digitalRoot n =
  -- TODO
  0

subsequences : List a -> List (List a)
subsequences xs =
  -- TODO
  []

take : Int -> List a -> Result String (List a)
take k xs =
  -- TODO
  Err "..."
