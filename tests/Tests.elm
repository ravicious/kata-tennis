module Tests exposing (all)

import Test exposing (..)
import Expect
import CustomFuzzers
import Tennis exposing (..)


-- Helper functions


isPoints : Score -> Bool
isPoints score =
    case score of
        Points _ ->
            True

        _ ->
            False


isForty : Score -> Bool
isForty score =
    case score of
        Forty _ ->
            True

        _ ->
            False


isDeuce : Score -> Bool
isDeuce score =
    case score of
        Deuce ->
            True

        _ ->
            False


isAdvantage : Score -> Bool
isAdvantage score =
    case score of
        Advantage _ ->
            True

        _ ->
            False


isGame : Score -> Bool
isGame score =
    case score of
        Game _ ->
            True

        _ ->
            False



-- Tests


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
                scoreWhenForty current.player current
                    |> Expect.equal (Game current.player)
        , fuzz CustomFuzzers.fortyData "Given player: 40 - other: 30 when other wins then score is correct" <|
            \current ->
                scoreWhenForty (other current.player) { current | otherPlayerPoint = Thirty }
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
                        scoreWhenForty (other current.player) current

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
                        scoreWhenPoints winner current

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
                        scoreWhenPoints winner current

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
          fuzz2 CustomFuzzers.player CustomFuzzers.score "score doesn't crash" <|
            \winner current ->
                let
                    actual =
                        score winner current
                in
                    Expect.true "Expected the score function not to crash" True
        , fuzz CustomFuzzers.wins "A game with less than four balls isn't over" <|
            \wins ->
                let
                    actual =
                        List.take 3 wins |> scoreList
                in
                    Expect.true "Expected the value not to be of Game constructor" <|
                        (actual |> (not << isGame))
        , fuzz CustomFuzzers.wins "A game with less than six balls can't be Deuce" <|
            \wins ->
                let
                    actual =
                        List.take 5 wins |> scoreList
                in
                    Expect.true "Expected the value not to be of Deuce constructor" <|
                        (actual |> (not << isDeuce))
        , fuzz
            CustomFuzzers.wins
            "A game with less than seven balls can't have any player with advantage"
          <|
            \wins ->
                let
                    actual =
                        List.take 6 wins |> scoreList
                in
                    Expect.true "Expected the value not to be of Advantage constructor" <|
                        (actual |> (not << isAdvantage))
        , fuzz
            (CustomFuzzers.moreThanNWins 4)
            "A game with more than four balls can't be Points"
          <|
            \wins ->
                let
                    actual =
                        scoreList wins
                in
                    Expect.true "Expected the value not to be of Points constructor" <|
                        (actual |> (not << isPoints))
        , fuzz
            (CustomFuzzers.moreThanNWins 5)
            "A game with more than five balls can't be Forty"
          <|
            \wins ->
                let
                    actual =
                        scoreList wins
                in
                    Expect.true "Expected the value not to be of Forty constructor" <|
                        (actual |> (not << isForty))
        , fuzz
            CustomFuzzers.player
            "A game where one player wins all balls is over in four balls"
          <|
            \winner ->
                let
                    fourWins =
                        List.repeat 4 winner

                    actual =
                        scoreList fourWins
                in
                    Expect.equal (Game winner) actual
        , fuzz CustomFuzzers.alternateWins "A game where players alternate never ends" <|
            \alternateWins ->
                let
                    actual =
                        scoreList alternateWins
                in
                    Expect.true "Expected the value not to be of Game constructor" <|
                        (actual |> (not << isGame))
        ]


otherTests : Test
otherTests =
    describe "Tests for the other function"
        [ fuzz CustomFuzzers.player "other returns a different player" <|
            \player ->
                Expect.notEqual player (other player)
        , fuzz CustomFuzzers.player "Calling other twice returns the same player" <|
            \player ->
                player
                    |> other
                    >> other
                    |> Expect.equal player
        ]


all : Test
all =
    concat [ transitionTests, scoreTests, otherTests ]
