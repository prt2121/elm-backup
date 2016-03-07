module Tests (..) where

import ElmTest exposing (..)
import Reader exposing (..)
import Combine exposing (..)
import Types exposing (..)
import Combine.Char as Char exposing (..)
import Combine.Infix exposing ((<$>), (*>), (<|>))


spacesSuite : Test
spacesSuite =
  suite
    "space tests"
    [ test "spaces test 1"
        <| assertEqual
            (parse spaces "   ")
            ( Ok (), { input = "", position = 3 } )
    , test "spaces test 2"
        <| assertEqual
            (parse spaces " , ")
            ( Ok (), { input = "", position = 3 } )
    , test "spaces test 3"
        <| assertEqual
            (parse spaces "  \n ")
            ( Ok (), { input = "", position = 4 } )
    , test "spaces test 4"
        <| assertEqual
            (parse spaces "  ,\n x")
            ( Ok (), { input = "x", position = 5 } )
    ]


commentSuite : Test
commentSuite =
  suite
    "comment tests"
    [ test "comment test 1"
        <| assertEqual
            (parse comment ";comment\x0D\n")
            ( Ok (), { input = "\x0D\n", position = 8 } )
    ]


ignoredSuite : Test
ignoredSuite =
  suite
    "ignored tests"
    [ test "ignored test 1"
        <| assertEqual
            (parse ignored "    ;comment\x0D\n")
            ( Ok (), { input = "\x0D\n", position = 12 } )
    , test "ignored test 2"
        <| assertEqual
            (parse ignored "  , \n ;ignored this ")
            ( Ok (), { input = "", position = 20 } )
    , test "ignored test 3"
        <| assertEqual
            (parse ignored "  , \n ;ignored this \x0D\nLOL")
            ( Ok (), { input = "\x0D\nLOL", position = 20 } )
    ]


readNumberSuite : Test
readNumberSuite =
  suite
    "read_number tests"
    [ test "read_number test 1"
        <| assertEqual
            (parse read_number "12")
            ( Ok (MalNumber 12), { input = "", position = 2 } )
    , test "read_number test 2"
        <| assertEqual
            (parse read_number "0")
            ( Ok (MalNumber 0), { input = "", position = 1 } )
    ]


readNegativeNumberSuite : Test
readNegativeNumberSuite =
  suite
    "read_negative_number tests"
    [ test "read_negative_number test 1"
        <| assertEqual
            (parse read_negative_number "-12")
            ( Ok (MalNumber -12), { input = "", position = 3 } )
    , test "read_negative_number test 2"
        <| assertEqual
            (parse read_negative_number "-1")
            ( Ok (MalNumber -1), { input = "", position = 2 } )
    ]


letterTestSuite : Test
letterTestSuite =
  suite
    "letter tests"
    [ test "letter test 1"
        <| assertEqual
            (parse letter "test")
            ( Ok 't', { input = "est", position = 1 } )
    , test "read_negative_number test 2"
        <| assertEqual
            (parse letter "Test")
            ( Ok 'T', { input = "est", position = 1 } )
    ]


keywordTestSuite : Test
keywordTestSuite =
  suite
    "read_keyword tests"
    [ test "read_keyword test 1"
        <| assertEqual
            (parse read_keyword ":keyword")
            ( Ok (MalString "ʞkeyword"), { input = "", position = 8 } )
    ]


all : Test
all =
  suite
    "test suite"
    [ spacesSuite
    , commentSuite
    , ignoredSuite
    , readNumberSuite
    , readNegativeNumberSuite
    , letterTestSuite
    , keywordTestSuite
    ]



-- elm make TestRunner.elm --output raw-test.js
-- ./elm-io.sh raw-test.js test.js
-- node test.js
