module Main exposing (..)

import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Time exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Dir
    = Left
    | Up
    | Right
    | Down


type alias Model =
    { width : Int
    , height : Int
    , antX : Int
    , antY : Int
    , antDir : Dir
    , blackSquares : List ( Int, Int )
    }


init : ( Model, Cmd Msg )
init =
    ( { width = 20
      , height = 20
      , antX = 10
      , antY = 10
      , antDir = Right
      , blackSquares = []
      }
    , Cmd.none
    )



-- UPDATE


subscriptions : Model -> Sub Msg
subscriptions model =
    every (20 * millisecond) Step


turnRight : Dir -> Dir
turnRight d =
    case d of
        Right ->
            Down

        Down ->
            Left

        Left ->
            Up

        Up ->
            Right


turnLeft : Dir -> Dir
turnLeft d =
    case d of
        Right ->
            Up

        Down ->
            Right

        Left ->
            Down

        Up ->
            Left


type Msg
    = Step Time


isBlackSquare : ( Int, Int ) -> List ( Int, Int ) -> Bool
isBlackSquare ( x, y ) blackSquares =
    List.any (\( xp, yp ) -> xp == x && yp == y) blackSquares


step : ( Int, Int ) -> Dir -> ( Int, Int )
step ( x, y ) dir =
    case dir of
        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )

        Up ->
            ( x, y - 1 )

        Down ->
            ( x, y + 1 )


flipSquare : ( Int, Int ) -> List ( Int, Int ) -> List ( Int, Int )
flipSquare ( x, y ) blackSquares =
    if isBlackSquare ( x, y ) blackSquares then
        List.filter (\( xp, yp ) -> not (xp == x && yp == y)) blackSquares
    else
        ( x, y ) :: blackSquares


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step _ ->
            -- If ant is on a white square:
            -- Turn 90deg right, flip color of current square, move forward one unit
            -- If ant is on a black square:
            -- Turn 90deg left, flip color of current square, move foward one unit
            if isBlackSquare ( model.antX, model.antY ) model.blackSquares then
                let
                    nextDir =
                        turnLeft model.antDir

                    nextBlacks =
                        flipSquare ( model.antX, model.antY ) model.blackSquares

                    ( nextX, nextY ) =
                        step ( model.antX, model.antY ) nextDir
                in
                    ( { model | antX = nextX, antY = nextY, antDir = nextDir, blackSquares = nextBlacks }, Cmd.none )
            else
                let
                    nextDir =
                        turnRight model.antDir

                    nextBlacks =
                        flipSquare ( model.antX, model.antY ) model.blackSquares

                    ( nextX, nextY ) =
                        step ( model.antX, model.antY ) nextDir
                in
                    ( { model | antX = nextX, antY = nextY, antDir = nextDir, blackSquares = nextBlacks }, Cmd.none )



-- VIEW


cellColor : Int -> Int -> Int -> Int -> List ( Int, Int ) -> String
cellColor r c antX antY blackSquares =
    let
        isBlack =
            isBlackSquare ( c, r ) blackSquares
    in
        if c == antX && r == antY then
            if isBlack then
                "#800"
            else
                "#f00"
        else if isBlack then
            "black"
        else
            "white"


cellStyle : Int -> Int -> Int -> Int -> List ( Int, Int ) -> List ( String, String )
cellStyle r c antX antY blackSquares =
    [ ( "border", "1px solid #ccc" )
    , ( "width", "20px" )
    , ( "height", "20px" )
    , ( "background-color", cellColor r c antX antY blackSquares )
    ]


rowStyle : Int -> List ( String, String )
rowStyle r =
    [ ( "position", "absolute" )
    , ( "display", "flex" )
    , ( "flex-direction", "row" )
    , ( "top", (toString (r * 20)) ++ "px" )
    ]


view : Model -> Html Msg
view model =
    let
        rows =
            List.range 1 model.height

        cols =
            List.range 1 model.width

        viewCell =
            \r c ->
                div [ style (cellStyle r c model.antX model.antY model.blackSquares) ] []

        viewRow =
            \r ->
                div [ style (rowStyle r) ] (List.map (viewCell r) cols)
    in
        div []
            (List.map viewRow rows)
