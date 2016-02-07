module AboutResults (..) where

import ElmTest exposing (..)
import Result
import TestHelpers exposing (..)


testSuite =
  suite
    "About Result"
    [ test
        "results represent the result of a function"
        (assertEqual (Ok 42) (Ok 42))
    , test
        "but could be an error"
        (assertEqual (Err "there was an error") (Err "there was an error"))
    , test
        "a result can be converted to a maybe"
        (assertEqual (Just 42) (Result.toMaybe (Ok 42)))
    , test
        "but an error will become nothing"
        (assertEqual Nothing (Result.toMaybe (Err "there was an error")))
    , test
        "a maybe can also be converted to a result"
        (assertEqual (Ok 42) (Result.fromMaybe "there was an error" (Just 42)))
    , test
        "and will become an error if there is nothing"
        (assertEqual (Err "there was an error") (Result.fromMaybe "there was an error" Nothing))
    ]
