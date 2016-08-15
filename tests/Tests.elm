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
        , fuzz CustomFuzzers.player "Given advantage when advantaged player wins then score is correct" <|
            \advantagedPlayer ->
                Tennis.scoreWhenAdvantage advantagedPlayer advantagedPlayer
                    |> Expect.equal (Tennis.Game advantagedPlayer)
        , fuzz CustomFuzzers.player "Given advantage when other player wins the score is correct" <|
            \advantagedPlayer ->
                Tennis.scoreWhenAdvantage advantagedPlayer (Tennis.other advantagedPlayer)
                    |> Expect.equal Tennis.Deuce
        ]
