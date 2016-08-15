module Tests exposing (..)

import Test exposing (..)
import Expect
import CustomFuzzers
import Tennis


all : Test
all =
    describe "A Test Suite for the Tennis module"
        [ fuzz CustomFuzzers.player "Given deuce when player wins then score is correct" <|
            \winner ->
                winner
                    |> Tennis.scoreWhenDeuce
                    |> Expect.equal (Tennis.Advantage winner)
        ]
