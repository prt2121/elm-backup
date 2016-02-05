module AboutDictionaries (..) where

import ElmTest exposing (..)
import Dict
import TestHelpers exposing (..)


testSuite =
  suite
    "About Dictionaries"
    <| [ test
          "dictionaries can be empty"
          (assertEqual 0 (Dict.size Dict.empty))
       , test
          "or initialized with a single key-value pair"
          (assertEqual 1 (Dict.size <| Dict.singleton 1 "a"))
       , test
          "or from a list of key-value pairs"
          (assertEqual 2 (Dict.size <| Dict.fromList [ ( 1, "a" ), ( 2, "b" ) ]))
       , test
          "they can also be converted back to a list"
          (assertEqual [ ( 1, "a" ), ( 2, "b" ) ] (Dict.toList <| Dict.fromList [ ( 1, "a" ), ( 2, "b" ) ]))
       , test
          "checking for an empty dictionary is easy"
          (assertEqual True (Dict.isEmpty Dict.empty))
       , test
          "you can also check if a key is present in the dictionary"
          (assertEqual True (Dict.member 1 <| Dict.singleton 1 "a"))
       , test
          "or get the value associated with the key"
          (assertEqual (Just "a") (Dict.get 1 <| Dict.singleton 1 "a"))
       , test
          "a key-value pair can be added to the dictionary"
          (assertEqual (Just "b") (Dict.get 2 <| Dict.insert 2 "b" <| Dict.singleton 1 "a"))
       , test
          "inserting can also overwrite the value associated with a key"
          (assertEqual (Just "b") (Dict.get 1 <| Dict.insert 1 "b" <| Dict.singleton 1 "a"))
       , test
          "updating a value works similarly"
          (assertEqual (Just "b") (Dict.get 1 <| Dict.update 1 (\_ -> Just "b") <| Dict.singleton 1 "a"))
       , test
          "but knows about the current value"
          (assertEqual (Just "ab") (Dict.get 1 <| Dict.update 1 (\v -> (Maybe.withDefault "" v) ++ "b" |> Just) <| Dict.singleton 1 "a"))
       , test
          "updating can add a new value"
          (assertEqual (Just "b") (Dict.get 2 <| Dict.update 2 (\_ -> Just "b") <| Dict.singleton 1 "a"))
       , test
          "or even remove a value"
          (assertEqual Nothing (Dict.get 1 <| Dict.update 1 (\_ -> Nothing) <| Dict.singleton 1 "a"))
       , test
          "of course there is a more direct way to remove a value too"
          (assertEqual Nothing (Dict.get 1 <| Dict.remove 1 <| Dict.singleton 1 "a"))
       , test
          "you can get the list of keys"
          (assertEqual [ 1 ] (Dict.keys <| Dict.singleton 1 "a"))
       , test
          "or the list of values"
          (assertEqual [ "a" ] (Dict.values <| Dict.singleton 1 "a"))
       , test
          "you can get the union of two dictionaries"
          (assertEqual [ "a", "b", "d" ] (Dict.values <| Dict.union (Dict.fromList [ ( 1, "a" ), ( 2, "b" ) ]) (Dict.fromList [ ( 2, "c" ), ( 3, "d" ) ])))
       , test
          "or the intersection"
          (assertEqual [ "b" ] (Dict.values <| Dict.intersect (Dict.fromList [ ( 1, "a" ), ( 2, "b" ) ]) (Dict.fromList [ ( 2, "c" ), ( 3, "d" ) ])))
       , test
          "or the difference"
          (assertEqual [ "a" ] (Dict.values <| Dict.diff (Dict.fromList [ ( 1, "a" ), ( 2, "b" ) ]) (Dict.fromList [ ( 2, "c" ), ( 3, "d" ) ])))
       , test
          "mapping works similarly as with lists"
          -- so does filter, foldl, foldr, partition
          (assertEqual [ "ac", "bc" ] (Dict.values <| Dict.map (\k v -> v ++ "c") (Dict.fromList [ ( 1, "a" ), ( 2, "b" ) ])))
       ]
