module Reader (..) where

import Combine exposing (..)
import Combine.Char as Char exposing (..)
import Combine.Infix exposing ((<$>), (*>), (<|>))
import String exposing (toList)
import Types exposing (..)


-- read_str :: String -> Result MalVal
-- read_str str =
--   case parse read_form


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
  Combine.map stringToMalNum <| many1 digit


read_negative_number : Parser MalVal
read_negative_number =
  char '-'
    `andThen` \sign ->
                many1 digit
                  `andThen` \n ->
                              sign
                                :: n
                                |> stringToMalNum
                                |> succeed


stringToMalNum : List Char -> Types.MalVal
stringToMalNum =
  String.fromList
    >> String.toInt
    >> Result.toMaybe
    >> Maybe.withDefault 0
    >> MalNumber



-- Combine.Parser (List Char)
-- many (Reader.escaped <|> (noneOf <| String.toList "\\\""))


read_string : Parser MalVal
read_string =
  char '"'
    *> many (escaped <|> (noneOf <| String.toList "\\\""))
    `andThen` \x ->
                char '"'
                  *> (String.fromList x |> MalString |> succeed)


read_symbol : Parser MalVal
read_symbol =
  (lower <|> upper <|> symbol)
    `andThen` \first ->
                many (lower <|> upper <|> digit <|> symbol)
                  `andThen` \rest ->
                              let
                                str =
                                  String.fromList <| first :: rest
                              in
                                succeed
                                  <| case str of
                                      "true" ->
                                        MalTrue

                                      "false" ->
                                        MalFalse

                                      "nil" ->
                                        Nil

                                      _ ->
                                        MalSymbol str



{- -
keyword: a keyword is a token that begins with a colon.
A keyword can just be stored as a string with special unicode prefix like 0x29E
- TODO: letter
-}
-- read_keyword : Parser MalVal
-- read_keyword =
--   char ':'
--     *> many (letter <|> digit <|> symbol)
--       `andThen` \x ->
--         succeed <| MalString <| "\x029e" ++ x


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


read_list : Parser MalVal
read_list =
  char '('
    *> sepEndBy ignored read_form
    `andThen` \x -> char ')' *> succeed (MalList x Nil)


read_vector : Parser MalVal
read_vector =
  char '['
    *> sepEndBy ignored read_form
    `andThen` \x -> char ']' *> succeed (MalList x Nil)



-- read_quasiquote : Parser MalVal
-- read_quasiquote =
--   char '`'
--     *> read_form
--     `andThen` \x -> succeed (MalList (List MalSymbol "quasiquote", x) Nil)


sepEndBy : Parser x -> Parser res -> Parser (List res)
sepEndBy sep p =
  sepEndBy1 sep p `or` succeed []


sepEndBy1 : Parser x -> Parser res -> Parser (List res)
sepEndBy1 sep p =
  p
    `andThen` \x ->
                (sep
                  *> sepEndBy sep p
                  `andThen` \xs ->
                              succeed (x :: xs)
                )
                  `or` succeed [ x ]
