-- https://www.classes.cs.uchicago.edu/archive/2015/winter/22300-1/assignments/HW1.html
-- http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Result
module FP where

import List exposing ((::), sum, concatMap, filter, isEmpty, foldr, map, length)
import List
import Result

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
  if n < 10 then 0
  else 1 + additivePersistence (sum(digitsOfInt n))

digitalRoot : Int -> Int
digitalRoot n =
  if n < 10 then n
  else digitalRoot(sum(digitsOfInt n))

inits : List a -> List (List a)
inits = foldr (\e acc -> []::map ((::)e) acc) [[]]

tails : List a -> List (List a)
tails = foldr tailsHelp [[]]

tailsHelp : a -> List (List a) -> List (List a)
tailsHelp e list =
  case list of
    (x::xs) ->
      (e::x)::x::xs
    [] ->
      []

subsequences : List a -> List (List a)
subsequences = (tails >> concatMap inits) >> filter (isEmpty >> not)

take : Int -> List a -> Result String (List a)
take k xs =
  if k <= length xs then Ok (List.take k xs)
  else Err "Yours is too short."
