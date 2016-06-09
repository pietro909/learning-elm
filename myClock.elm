import Html exposing (Html)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)
import Date exposing (..)
import Debug


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL

type alias Model = Date

init : (Model, Cmd Msg)
init =
    (Date.fromTime 1.0, Cmd.none)


-- UPDATE

type Msg
    = Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
    case action of
        Tick newTime ->
            let 
                date = Date.fromTime newTime
            in
                (date, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.second Tick


-- VIEW

view : Model -> Html Msg
view model =
    let
        seconds = toFloat(Date.second model)
        minutes = toFloat(Date.minute model)
        hours = toFloat(5 * (Date.hour model))
    in
        svg [ viewBox "0 0 100 100", width "300px" ]
            [ circle [ cx "50", cy "50", r "45", fill "#0B79CE" ] []
            , buildNeedle seconds 40
            , buildNeedle minutes 35
            , buildNeedle hours 30
            ]

buildNeedle : Float -> Int -> Html Msg
buildNeedle unit size =
    let
        turn = unit / 60 - 0.25
        angle = turns turn
        handX = toString (50 + toFloat(size) * cos angle)
        handY = toString (50 + toFloat(size) * sin angle)
    in
       line [ x1 "50", y1 "50", x2 handX, y2 handY, stroke "#023963" ] []

