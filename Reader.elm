module Reader (..) where

import Combine exposing (..)
import Combine.Char as Char exposing (..)
import Combine.Infix exposing ((<$>), (*>), (<|>))
import String exposing (toList)
import Types exposing (..)


-- read_str :: String -> Result


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
                      succeed x


read_number : Parser MalVal
read_number =
  let
    stringToMalNum =
      String.fromList
        >> String.toInt
        >> Result.toMaybe
        >> Maybe.withDefault 0
        >> MalNumber
  in
    Combine.map stringToMalNum <| many1 digit



-- Combine.Parser (List Char)
-- many (Reader.escaped <|> (noneOf <| String.toList "\\\""))


read_string : Parser MalVal
read_string =
  char '"'
    *> many (escaped <|> (noneOf <| String.toList "\\\""))
    `andThen` \x ->
                char '"'
                  *> (String.fromList x |> MalString |> succeed)


read_atom : Parser MalVal
read_atom =
  read_number
    <|> read_string


read_form : Parser MalVal
read_form =
  ignored
    *> read_atom
    `andThen` \x ->
                succeed x
