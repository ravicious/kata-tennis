module Tennis exposing (..)


type Player
    = PlayerOne
    | PlayerTwo


type Point
    = Love
    | Fifteen
    | Thirty


type alias PointsData =
    { playerOnePoint : Point
    , playerTwoPoint : Point
    }


type alias FortyData =
    { player : Player
    , otherPlayerPoint : Point
    }


type Score
    = Points PointsData
    | Forty FortyData
    | Deuce
    | Advantage Player
    | Game Player


scoreWhenDeuce : Player -> Score
scoreWhenDeuce winner =
    Advantage winner


scoreWhenAdvantage : Player -> Player -> Score
scoreWhenAdvantage advantagedPlayer winner =
    if advantagedPlayer == winner then
        Game winner
    else
        Deuce


other : Player -> Player
other player =
    case player of
        PlayerOne ->
            PlayerTwo

        PlayerTwo ->
            PlayerOne
