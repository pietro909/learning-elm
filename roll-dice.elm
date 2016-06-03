import Html exposing (Html, button, div, text, h1)
import Html.App as Html
import Html.Events exposing (onClick)
import Random

main = 
    Html.program 
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- MODEL
type alias Model =
    { dieOne : Int
    , dieTwo : Int
    }

-- INIT
init : (Model, Cmd Msg)
init =
    (Model 1 1, Cmd.none)

-- UPDATE
type Msg 
    = Roll
    | NewFace (Int, Int)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Roll ->
            (model, Random.generate NewFace (Random.pair (Random.int 1 6) (Random.int 1 6)))
        NewFace (dieOne, dieTwo) ->
            (Model dieOne dieTwo, Cmd.none)

-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (toString model.dieOne) ]
        , h1 [] [ text (toString model.dieTwo) ]
        , button [ onClick Roll ] [ text "Roll" ]
        ]

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

