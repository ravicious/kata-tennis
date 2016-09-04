module CustomFuzzers
    exposing
        ( player
        , fortyData
        , loveOrFifteen
        , pointsData
        , score
        , wins
        , moreThanNWins
        , alternateWins
        )

import Fuzz
import Tennis exposing (..)
import Random.Pcg as Random
import Lazy.List exposing ((:::), empty)
import Shrink


-- Generators


playerGenerator : Random.Generator Player
playerGenerator =
    Random.choice PlayerOne PlayerTwo


pointGenerator : Random.Generator Point
pointGenerator =
    Random.sample [ Love, Fifteen, Thirty ]
        |> Random.map (Maybe.withDefault Love)


fortyDataGenerator : Random.Generator FortyData
fortyDataGenerator =
    Random.map2 FortyData playerGenerator pointGenerator


pointsDataGenerator : Random.Generator PointsData
pointsDataGenerator =
    Random.map2 PointsData pointGenerator pointGenerator


fortyGenerator : Random.Generator Score
fortyGenerator =
    Random.map Forty fortyDataGenerator


pointsGenerator : Random.Generator Score
pointsGenerator =
    Random.map Points pointsDataGenerator


deuceGenerator : Random.Generator Score
deuceGenerator =
    Random.constant Deuce


advantageGenerator : Random.Generator Score
advantageGenerator =
    Random.map Advantage playerGenerator


gameGenerator : Random.Generator Score
gameGenerator =
    Random.map Game playerGenerator


scoreGenerator : Random.Generator Score
scoreGenerator =
    Random.choices
        [ pointsGenerator
        , fortyGenerator
        , deuceGenerator
        , advantageGenerator
        , gameGenerator
        ]


winsGenerator : Int -> Int -> Random.Generator Wins
winsGenerator min max =
    Random.andThen
        (Random.int min max)
        (\n -> Random.list n playerGenerator)


{-| Franc Klaassen and Jan Magnus in "On the Independence and Identical Distribution of Points in
Tennis" (1998) state that at Wimbledon from 1992 to 1995 the average number of points in game
was 6.12 for men's singles and 6.46 for ladies' singles. The average number of points in tiebreak
was 12.13 and 11.84 respectively (Table 1 in the paper).

Link to the paper:
http://econpapers.repec.org/paper/tiutiucen/395a6222-6318-49b5-a42c-643d0ee833a0.htm
-}
averageNumberOfPointsInGame : Int
averageNumberOfPointsInGame =
    6


{-| Multiplying the average by three seems to result in a reasonably high upper bound for the number
of points in a game to generate.
-}
upperBoundForNumberOfWins : Int
upperBoundForNumberOfWins =
    averageNumberOfPointsInGame * 3


randomWinsGenerator : Random.Generator Wins
randomWinsGenerator =
    winsGenerator 0 upperBoundForNumberOfWins


moreThanNWinsGenerator : Int -> Random.Generator Wins
moreThanNWinsGenerator n =
    let
        lowerBound =
            n + 1

        upperBound =
            -- If the lower bound is equal or greater than the upper bound, it doesn't make sense
            -- to generate a range with such bounds. Thus we increase the upper bound.
            if lowerBound >= upperBoundForNumberOfWins then
                upperBoundForNumberOfWins + n
            else
                upperBoundForNumberOfWins
    in
        winsGenerator lowerBound upperBound


alternateWinsGenerator : Random.Generator Wins
alternateWinsGenerator =
    Random.andThen
        (Random.int 0 (upperBoundForNumberOfWins // 2))
        (\n ->
            playerGenerator
                |> Random.map (\player -> [ player, (other player) ])
                |> (flip Random.andThen) (\players -> Random.list n (Random.constant players))
                |> Random.map List.concat
        )



-- Shrinkers


playerShrinker : Player -> Lazy.List.LazyList Player
playerShrinker player =
    case player of
        PlayerOne ->
            empty

        PlayerTwo ->
            PlayerOne ::: empty


pointShrinker : Point -> Lazy.List.LazyList Point
pointShrinker point =
    case point of
        Love ->
            empty

        Fifteen ->
            Love ::: empty

        Thirty ->
            Fifteen ::: Love ::: empty


fortyDataShrinker : Shrink.Shrinker FortyData
fortyDataShrinker { player, otherPlayerPoint } =
    FortyData
        `Shrink.map` playerShrinker player
        `Shrink.andMap` pointShrinker otherPlayerPoint


pointsDataShrinker : Shrink.Shrinker PointsData
pointsDataShrinker { playerOnePoint, playerTwoPoint } =
    PointsData
        `Shrink.map` pointShrinker playerOnePoint
        `Shrink.andMap` pointShrinker playerTwoPoint


{-| Shrinks a Score value, but only within a particular constructor.
This means that, for example, a Deuce won't be shrinked to Forty
and Forty won't be shrinked to Points.

This way of shrinking is enough for our use case.
-}
scoreShrinker : Shrink.Shrinker Score
scoreShrinker score =
    case score of
        Points pointsData ->
            pointsDataShrinker pointsData |> Shrink.map Points

        Forty fortyData ->
            fortyDataShrinker fortyData |> Shrink.map Forty

        Deuce ->
            Shrink.noShrink Deuce

        Advantage advantagedPlayer ->
            playerShrinker advantagedPlayer |> Shrink.map Advantage

        Game winner ->
            playerShrinker winner |> Shrink.map Game


winsShrinker : Shrink.Shrinker Wins
winsShrinker =
    Shrink.list playerShrinker


{-| Given a list of players, shrinks it until the length of the list is equal to lowerBound plus 1.
Usually used together with moreThanNWinsGenerator.

Had moreThanNWins used winsShrinker instead, moreThanNWinsGenerator would be generating lists longer
than the lowerBound, but then winsShrinker would be shrinking the lists to a size smaller
than lowerBound.
-}
moreThanNWinsShrinker : Int -> Shrink.Shrinker Wins
moreThanNWinsShrinker lowerBound =
    Shrink.keepIf (\wins -> (List.length wins) > lowerBound) winsShrinker


{-| A list with less than two elements doesn't have alternate elements.
-}
minAlternateWinsListLength : Int
minAlternateWinsListLength =
    2


{-| If the list has more than two elements, returns a lazy list with the tail of the list.
Otherwise returns an empty lazy list – a list of alternate wins can't have less than two elements.

Since this shrinker is called only on lists with alternate elements, that property of them
(having alternate elements) is kept after removing the first element.
-}
alternateWinsShrinker : Shrink.Shrinker Wins
alternateWinsShrinker list =
    if List.length list > minAlternateWinsListLength then
        list
            |> List.tail
            |> Maybe.withDefault []
            |> Lazy.List.singleton
    else
        empty



-- Fuzzers


player : Fuzz.Fuzzer Player
player =
    Fuzz.custom playerGenerator playerShrinker


wins : Fuzz.Fuzzer Wins
wins =
    Fuzz.custom randomWinsGenerator winsShrinker


moreThanNWins : Int -> Fuzz.Fuzzer Wins
moreThanNWins n =
    Fuzz.custom (moreThanNWinsGenerator n) (moreThanNWinsShrinker n)


alternateWins : Fuzz.Fuzzer Wins
alternateWins =
    Fuzz.custom alternateWinsGenerator alternateWinsShrinker


fortyData : Fuzz.Fuzzer FortyData
fortyData =
    Fuzz.custom fortyDataGenerator fortyDataShrinker


loveOrFifteen : Fuzz.Fuzzer Point
loveOrFifteen =
    let
        generator =
            Random.choice Love Fifteen

        shrinker =
            pointShrinker
    in
        Fuzz.custom generator shrinker


pointsData : Fuzz.Fuzzer PointsData
pointsData =
    Fuzz.custom pointsDataGenerator pointsDataShrinker


score : Fuzz.Fuzzer Score
score =
    Fuzz.custom scoreGenerator scoreShrinker
