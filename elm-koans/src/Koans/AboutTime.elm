module AboutTime (..) where

import ElmTest exposing (..)
import TestHelpers exposing (..)
import Time


testSuite =
  suite
    "About Time"
    [ test
        "time is just a Float"
        (assertEqual 123.45 123.45)
    , test
        "a constant exists for hour"
        (assertEqual 3600000 (1 * Time.hour))
    , test
        "and minute"
        (assertEqual 60000 (1 * Time.minute))
    , test
        "and second"
        (assertEqual 1000 (1 * Time.second))
    , test
        "and millisecond"
        (assertEqual 1 (1 * Time.millisecond))
    , test
        "helpers exist to convert back to Floats"
        (assertEqual 1 (Time.inSeconds 1000))
    ]
