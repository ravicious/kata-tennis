module Tests exposing (..)

import Test exposing (..)
import Expect
import CustomFuzzers
import Tennis exposing (..)


all : Test
all =
    describe "A Test Suite for the Tennis module"
        [ fuzz CustomFuzzers.player "Given deuce when player wins then score is correct" <|
            \winner ->
                winner
                    |> scoreWhenDeuce
                    |> Expect.equal (Advantage winner)
        , fuzz CustomFuzzers.player "Given advantage when advantaged player wins then score is correct" <|
            \advantagedPlayer ->
                scoreWhenAdvantage advantagedPlayer advantagedPlayer
                    |> Expect.equal (Game advantagedPlayer)
        , fuzz CustomFuzzers.player "Given advantage when other player wins the score is correct" <|
            \advantagedPlayer ->
                scoreWhenAdvantage advantagedPlayer (other advantagedPlayer)
                    |> Expect.equal Deuce
        , fuzz CustomFuzzers.fortyData "Given player: 40 when player wins then score is correct" <|
            \current ->
                scoreWhenForty current current.player
                    |> Expect.equal (Game current.player)
        , fuzz CustomFuzzers.fortyData "Given player: 40 - other: 30 when other wins then score is correct" <|
            \current ->
                scoreWhenForty { current | otherPlayerPoint = Thirty } (other current.player)
                    |> Expect.equal Deuce
        , fuzz2
            CustomFuzzers.fortyData
            CustomFuzzers.loveOrFifteen
            "Given player: 40 - other: < 30 when other wins then score is correct"
          <|
            \current otherPlayerPoint ->
                let
                    current =
                        { current | otherPlayerPoint = otherPlayerPoint }

                    actual =
                        scoreWhenForty current (other current.player)

                    expected =
                        incrementPoint current.otherPlayerPoint
                            |> Maybe.map (\nextPoint -> { current | otherPlayerPoint = nextPoint })
                            |> Maybe.map Forty
                in
                    Expect.equal expected (Just actual)
        , fuzz2
            CustomFuzzers.pointsData
            CustomFuzzers.player
            "Given player: 30 when player wins then score is correct"
          <|
            \current winner ->
                let
                    current =
                        pointTo winner Thirty current

                    actual =
                        scoreWhenPoints current winner

                    expected =
                        Forty { player = winner, otherPlayerPoint = pointFor (other winner) current }
                in
                    Expect.equal expected actual
        , fuzz3
            CustomFuzzers.pointsData
            CustomFuzzers.player
            CustomFuzzers.loveOrFifteen
            "Given player: < 30 when player wins then score is correct"
          <|
            \current winner loveOrFifteen ->
                let
                    current =
                        pointTo winner loveOrFifteen current

                    actual =
                        scoreWhenPoints current winner

                    expectedPlayerPoint =
                        current
                            |> pointFor winner
                            |> incrementPoint

                    expected =
                        expectedPlayerPoint
                            |> Maybe.map (\point -> Points <| pointTo winner point current)
                in
                    Expect.equal expected (Just actual)
        ]
