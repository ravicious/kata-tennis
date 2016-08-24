module Tests exposing (all)

import Test exposing (..)
import Expect
import CustomFuzzers
import Tennis exposing (..)


transitionTests : Test
transitionTests =
    describe "Tests for specific transitions"
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
        , fuzz CustomFuzzers.player "Given game is over it stays over" <|
            \winner ->
                let
                    actual =
                        scoreWhenGame winner

                    expected =
                        Game winner
                in
                    Expect.equal expected actual
        ]


scoreTests : Test
scoreTests =
    describe "Tests for the score function"
        [ -- The following test is mostly a smoke test which checks
          -- if the function crashes for certain inputs.
          fuzz2 CustomFuzzers.score CustomFuzzers.player "score doesn't crash" <|
            \current winner ->
                let
                    actual =
                        score current winner
                in
                    Expect.true "The score function didn't crash" True
        ]


all : Test
all =
    concat [ transitionTests, scoreTests ]
