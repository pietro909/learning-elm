module Counter exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- MODEL
type alias Model =
    { value : Int
    , max : Int
    , min : Int
    }

init : Int -> Model
init count =
    Model count 0 0


-- UPDATE
type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            Model (model.value + 1) model.max model.min

        Decrement ->
            Model (model.value - 1) model.max model.min


-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button [ onClick Decrement ] [ text "-" ]
            , div [ countStyle ] [ text (toString model.value) ]
            , button [ onClick Increment ] [ text "+" ]
            ]
        , span [] [ text (toString model.min) ]
        , span [] [ text (toString model.max) ]
        ]

countStyle : Attribute msg
countStyle =
    style
        [ ("font-size", "20px")
        , ("font-family", "monospace")
        , ("display", "inline-block")
        , ("width", "50px")
        , ("text-align", "center")
        ]
