module CustomFuzzers exposing (player, fortyData, loveOrFifteen, pointsData, score)

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



-- Fuzzers


player : Fuzz.Fuzzer Player
player =
    Fuzz.custom playerGenerator playerShrinker


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
