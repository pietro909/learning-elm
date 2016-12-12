module Counter exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String


-- MODEL
type alias Model =
    { value : Int
    , max : Int
    , min : Int
    , ticks : Int
    }

init : Int -> Model
init count =
    Model count 0 0 0


-- UPDATE
type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            let
                value = model.value + 1
                max = if value > model.max then value else model.max
            in
                Model value max model.min (model.ticks + 1)

        Decrement ->
            let
                value = model.value - 1
                min = if value < model.min then value else model.min
            in
                Model value model.max min (model.ticks + 1)


-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick Decrement ] [ text "-" ]
            , div [ countStyle ] [ text (toString model.value) ]
            , button [ onClick Increment ] [ text "+" ]
            ]
        , div []
            [ showNumber "min: " model.min
            , showNumber "max: " model.max
            , showNumber "ticks: " model.ticks
            ]
        ]

showNumber : String -> Int -> Html Msg
showNumber title value =
    div []
        [ text (String.concat [title, " ", toString(value)]) ]


countStyle : Attribute msg
countStyle =
    style
        [ ("font-size", "20px")
        , ("font-family", "monospace")
        , ("display", "inline-block")
        , ("width", "50px")
        , ("text-align", "center")
        ]
