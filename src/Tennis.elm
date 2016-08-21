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


other : Player -> Player
other player =
    case player of
        PlayerOne ->
            PlayerTwo

        PlayerTwo ->
            PlayerOne


incrementPoint : Point -> Maybe Point
incrementPoint point =
    case point of
        Love ->
            Just Fifteen

        Fifteen ->
            Just Thirty

        Thirty ->
            Nothing


pointTo : Player -> Point -> PointsData -> PointsData
pointTo player point pointsData =
    case player of
        PlayerOne ->
            { pointsData | playerOnePoint = point }

        PlayerTwo ->
            { pointsData | playerTwoPoint = point }


pointFor : Player -> PointsData -> Point
pointFor player pointsData =
    case player of
        PlayerOne ->
            pointsData.playerOnePoint

        PlayerTwo ->
            pointsData.playerTwoPoint



-- Transitions


scoreWhenDeuce : Player -> Score
scoreWhenDeuce winner =
    Advantage winner


scoreWhenAdvantage : Player -> Player -> Score
scoreWhenAdvantage advantagedPlayer winner =
    if advantagedPlayer == winner then
        Game winner
    else
        Deuce


scoreWhenForty : FortyData -> Player -> Score
scoreWhenForty current winner =
    if current.player == winner then
        Game winner
    else
        case (incrementPoint current.otherPlayerPoint) of
            Just point ->
                Forty { current | otherPlayerPoint = point }

            Nothing ->
                Deuce


scoreWhenPoints : PointsData -> Player -> Score
scoreWhenPoints current winner =
    let
        nextWinnerPoint =
            incrementPoint (pointFor winner current)
    in
        case nextWinnerPoint of
            Just point ->
                Points <| pointTo winner point current

            Nothing ->
                Forty
                    { player = winner
                    , otherPlayerPoint = pointFor (other winner) current
                    }
