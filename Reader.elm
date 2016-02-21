module Reader (..) where

import Combine exposing (..)
import Combine.Char as Char exposing (..)
import Combine.Infix exposing ((<$>), (*>), (<|>))
import String exposing (toList)


spaces : Parser ()
spaces =
  toList ", \n"
    |> oneOf
    |> skipMany1



-- parse comment ";lol" =
-- (Ok (),{ input = "", position = 4 }) : ( Combine.Result (), Combine.Context )


comment : Parser ()
comment =
  char ';'
    *> (skipMany <| noneOf <| toList "\x0D\n")


ignored : Parser ()
ignored =
  skipMany (spaces <|> comment)


symbol : Parser Char
symbol =
  "!#$%&|*+-/:<=>?@^_~"
    |> toList
    |> oneOf


escaped : Parser Char
escaped =
  let
    slashQouteN =
      "\\\"n" |> toList |> oneOf
  in
    char '\\'
      *> slashQouteN
      `andThen` \x ->
                  case x of
                    'n' ->
                      char '\n'

                    _ ->
                      char x
