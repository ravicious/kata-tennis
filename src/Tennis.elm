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
