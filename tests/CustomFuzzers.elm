module CustomFuzzers exposing (player)

import Fuzz
import Tennis
import Random.Pcg as Random
import Lazy.List exposing ((:::), empty)


player : Fuzz.Fuzzer Tennis.Player
player =
    let
        generator =
            Random.choice Tennis.PlayerOne Tennis.PlayerTwo

        shrinker player =
            case player of
                Tennis.PlayerOne ->
                    empty

                Tennis.PlayerTwo ->
                    Tennis.PlayerOne ::: empty
    in
        Fuzz.custom generator shrinker
