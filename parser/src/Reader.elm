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
    *> (toList "\x0D\n"
          |> noneOf
          |> skipMany
       )


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
  stringToMalNum <$> many1 digit


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


stringToMalNum : List Char -> MalVal
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


letter : Parser Char
letter =
  lower <|> upper



{- -
keyword: a keyword is a token that begins with a colon.
A keyword can just be stored as a string with special unicode prefix like 0x29E
-}


read_keyword : Parser MalVal
read_keyword =
  char ':'
    *> many (letter <|> digit <|> symbol)
    `andThen` \x ->
                "ʞ"
                  ++ (String.fromList x)
                  |> MalString
                  |> succeed


read_atom : Parser MalVal
read_atom =
  read_number
    <|> read_string
    <|> read_keyword
    <|> read_symbol


read_macro : Parser MalVal
read_macro =
  read_quote
    <|> read_quasiquote
    <|> read_unquote
    <|> read_deref
    -- <|> read_with_meta
    -- <|> try read_splice_unquote

-- TODO https://github.com/elm-lang/elm-compiler/issues/873
read_form : Parser MalVal
read_form =
  ignored
    *> read_atom
    -- <|> read_macro
    -- <|> read_list
    -- <|> read_vector
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


read_quote : Parser MalVal
read_quote =
  char '\''
    *> read_form
    `andThen` \x -> succeed (MalList [ MalSymbol "quote", x ] Nil)


read_quasiquote : Parser MalVal
read_quasiquote =
  char '`'
    *> read_form
    `andThen` \x -> succeed (MalList [ MalSymbol "quasiquote", x ] Nil)


read_unquote : Parser MalVal
read_unquote =
  char '~'
    *> read_form
    `andThen` \x -> succeed (MalList [ MalSymbol "unquote", x ] Nil)


read_deref : Parser MalVal
read_deref =
  char '@'
    *> read_form
    `andThen` \x -> succeed (MalList [ MalSymbol "deref", x ] Nil)


read_with_meta : Parser MalVal
read_with_meta =
  char '^'
    *> sequence [ read_form, read_form ]
    `andThen` \ls -> succeed <| MalList (MalSymbol "with-meta" :: ls) Nil
