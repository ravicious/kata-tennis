module CustomFuzzers exposing (player, fortyData, loveOrFifteen)

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



-- Fuzzers


player : Fuzz.Fuzzer Player
player =
    Fuzz.custom playerGenerator playerShrinker


fortyData : Fuzz.Fuzzer FortyData
fortyData =
    let
        generator =
            Random.map2 FortyData playerGenerator pointGenerator
    in
        Fuzz.custom generator fortyDataShrinker


loveOrFifteen : Fuzz.Fuzzer Point
loveOrFifteen =
    let
        generator =
            Random.choice Love Fifteen

        shrinker =
            pointShrinker
    in
        Fuzz.custom generator shrinker
