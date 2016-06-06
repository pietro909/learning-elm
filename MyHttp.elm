module MyHttp exposing (..)

import Html exposing (Html, button, div, text, h1, h2, img)
import Html.App as Html
import Html.Events exposing (onClick)
import Html.Attributes exposing (src)
import Json.Decode as Json
import Task
import Http

main = 
    Html.program 
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- MODEL
type alias Model = 
    { topic : String
    , gifUrl : String
    }

init : (Model, Cmd Msg)
init =
    (Model "cats" "waiting.gif", Cmd.none)


-- UPDATE
type Msg
    = MorePlease
    | FetchSucceed String
    | FetchFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MorePlease ->
            (model, getRandomGif model.topic)

        FetchSucceed newUrl ->
            (Model model.topic newUrl, Cmd.none)

        FetchFail _ ->
            (model, Cmd.none)


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url =
            "http://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
    in
       Task.perform FetchFail FetchSucceed (Http.get decodeGifUrl url)

decodeGifUrl : Json.Decoder String
decodeGifUrl =
    Json.at ["data", "image_url"] Json.string


-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ h2 [] [text model.topic]
        , img [src model.gifUrl] []
        , button [ onClick MorePlease ] [ text "More Please!" ]
        ]


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

