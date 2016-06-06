module MyHttp exposing (..)

import Html exposing (Html, Attribute, button, div, text, h1, h2, img, input)
import Html.App as Html
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode 
import Task
import Http

main = 
    Html.program 
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- COMMON EVENTS
onKeyUp : (String -> msg) -> Attribute msg
onKeyUp tagger =
      Html.Events.on "keyup" (Json.Decode.map tagger Html.Events.targetValue)


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
    | NewTopic String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        MorePlease ->
            (model, getRandomGif model.topic)

        FetchSucceed newUrl ->
            (Model model.topic newUrl, Cmd.none)

        FetchFail _ ->
            (model, Cmd.none)

        NewTopic topic ->
            (Model topic "", Cmd.none)

getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url =
            "http://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
    in
       Task.perform FetchFail FetchSucceed (Http.get decodeGifUrl url)

decodeGifUrl : Json.Decode.Decoder String
decodeGifUrl =
    Json.Decode.at ["data", "image_url"] Json.Decode.string


-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ h2 [] [text model.topic]
        , img [src model.gifUrl] []
        , input [ type' "text", onKeyUp NewTopic ] []
        , button [ onClick MorePlease ] [ text "More Please!" ]
        ]


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

