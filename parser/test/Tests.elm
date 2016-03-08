module Tests (..) where

import ElmTest exposing (..)
import Reader exposing (..)
import Combine exposing (..)
import Types exposing (..)
import Combine.Char as Char exposing (..)
import Combine.Infix exposing ((<$>), (*>), (<|>))


isEqualTo =
  flip assertEqual


spacesSuite : Test
spacesSuite =
  suite
    "space tests"
    [ test "spaces test 1"
        <| (parse spaces "   ")
        `isEqualTo` ( Ok (), { input = "", position = 3 } )
    , test "spaces test 2"
        <| (parse spaces " , ")
        `isEqualTo` ( Ok (), { input = "", position = 3 } )
    , test "spaces test 3"
        <| (parse spaces "  \n ")
        `isEqualTo` ( Ok (), { input = "", position = 4 } )
    , test "spaces test 4"
        <| (parse spaces "  ,\n x")
        `isEqualTo` ( Ok (), { input = "x", position = 5 } )
    ]


commentSuite : Test
commentSuite =
  suite
    "comment tests"
    [ test "comment test 1"
        <| (parse comment ";comment\x0D\n")
        `isEqualTo` ( Ok (), { input = "\x0D\n", position = 8 } )
    ]


ignoredSuite : Test
ignoredSuite =
  suite
    "ignored tests"
    [ test "ignored test 1"
        <| (parse ignored "    ;comment\x0D\n")
        `isEqualTo` ( Ok (), { input = "\x0D\n", position = 12 } )
    , test "ignored test 2"
        <| (parse ignored "  , \n ;ignored this ")
        `isEqualTo` ( Ok (), { input = "", position = 20 } )
    , test "ignored test 3"
        <| (parse ignored "  , \n ;ignored this \x0D\nLOL")
        `isEqualTo` ( Ok (), { input = "\x0D\nLOL", position = 20 } )
    ]


readNumberSuite : Test
readNumberSuite =
  suite
    "read_number tests"
    [ test "read_number test 1"
        <| (parse read_number "12")
        `isEqualTo` ( Ok (MalNumber 12), { input = "", position = 2 } )
    , test "read_number test 2"
        <| (parse read_number "0")
        `isEqualTo` ( Ok (MalNumber 0), { input = "", position = 1 } )
    ]


readNegativeNumberSuite : Test
readNegativeNumberSuite =
  suite
    "read_negative_number tests"
    [ test "read_negative_number test 1"
        <| (parse read_negative_number "-12")
        `isEqualTo` ( Ok (MalNumber -12), { input = "", position = 3 } )
    , test "read_negative_number test 2"
        <| (parse read_negative_number "-1")
        `isEqualTo` ( Ok (MalNumber -1), { input = "", position = 2 } )
    ]


letterTestSuite : Test
letterTestSuite =
  suite
    "letter tests"
    [ test "letter test 1"
        <| (parse letter "test")
        `isEqualTo` ( Ok 't', { input = "est", position = 1 } )
    , test "letter test 2"
        <| (parse letter "Test")
        `isEqualTo` ( Ok 'T', { input = "est", position = 1 } )
    ]


keywordTestSuite : Test
keywordTestSuite =
  suite
    "read_keyword tests"
    [ test "read_keyword test 1"
        <| (parse read_keyword ":keyword")
        `isEqualTo` ( Ok (MalString "ʞkeyword"), { input = "", position = 8 } )
    , test "read_keyword test 2"
        <| (parse read_keyword ":123abc")
        `isEqualTo` ( Ok (MalString "ʞ123abc"), { input = "", position = 7 } )
    , test "read_keyword test 3"
        <| (parse read_keyword ":@cool! yo")
        `isEqualTo` ( Ok (MalString "ʞ@cool!"), { input = " yo", position = 7 } )
    ]


readListSuite : Test
readListSuite =
  suite
    "read_list tests"
    [ test "read_list test 1"
        <| (parse read_list "(1 2)")
        `isEqualTo` ( Ok (MalList [ MalNumber 1, MalNumber 2 ] Nil), { input = "", position = 5 } )
    , test "read_list test 2"
        <| (parse read_list "(3 A B 4)")
        `isEqualTo` ( Ok (MalList [ MalNumber 3, MalSymbol "A", MalSymbol "B", MalNumber 4 ] Nil), { input = "", position = 9 } )
    , test "read_list test 3"
        <| (parse read_list "(:@cool! A ?) XXX")
        `isEqualTo` ( Ok (MalList [ MalString "ʞ@cool!", MalSymbol "A", MalSymbol "?" ] Nil), { input = " XXX", position = 13 } )
    ]


quasiquoteTestSuite : Test
quasiquoteTestSuite =
  suite
    "quasiquote tests"
    [ test "quasiquote test 1"
        <| (parse read_quasiquote "`test")
        `isEqualTo` ( Ok (MalList [ MalSymbol "quasiquote", MalSymbol "test" ] Nil), { input = "", position = 5 } )
    , test "quasiquote test 2"
        <| (parse read_quasiquote "`:@cool! yo")
        `isEqualTo` ( Ok (MalList [ MalSymbol "quasiquote", MalString "ʞ@cool!" ] Nil), { input = " yo", position = 8 } )
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
    , readListSuite
    , quasiquoteTestSuite
    ]



-- elm make TestRunner.elm --output raw-test.js
-- ./elm-io.sh raw-test.js test.js
-- node test.js
