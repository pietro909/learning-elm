module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Array exposing (Array)
import Random

type alias Photo =
  { url : String }

type ThumbnailSize
  = Small
  | Medium
  | Large

type alias Model =
  { photos : List Photo
  , selectedUrl : Maybe String
  , loadingError : Maybe String
  , chosenSize : ThumbnailSize
  }

initialModel : Model
initialModel =
  { photos = [ ]
  , selectedUrl = Nothing
  , loadingError = Nothing
  , chosenSize = Medium
  }

type Msg
  = SelectByUrl String
  | SelectByIndex Int
  | SetSize ThumbnailSize
  | SurpriseMe

urlPrefix : String
urlPrefix = "http://elm-in-action.com/"

sizeToString : ThumbnailSize -> String
sizeToString size =
  case size of
    Small -> "small"
    Medium -> "med"
    Large -> "Large"

viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser current size =
  label []
    [ input
      [ type_ "radio"
      , name "size"
      , onClick (SetSize size)
      , checked (size == current)
      ] []
    , text (sizeToString size)
    ]

viewThumbnail : Maybe String -> Photo -> Html Msg
viewThumbnail selectedUrl thumbnail =
  img
    [ src (urlPrefix ++ thumbnail.url)
    , classList [ ( "selected", selectedUrl == Just thumbnail.url) ]
    , onClick (SelectByUrl thumbnail.url)
    ]
    []

viewLarge : Maybe String -> Html Msg
viewLarge maybeUrl =
  case maybeUrl of
    Nothing ->
      text ""
    Just url ->
      img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ url)
        ]
        []

view : Model -> Html Msg
view model =
  div [ class "content" ]
    [ h1 [] [ text "Photo Groove" ]
    , button
      [ onClick SurpriseMe ]
      [ text "Surprise me!" ]
    , h3 [] [ text "Thumbnail size:" ]
    , div [ id "choose-size" ]
      ( List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ] )
    , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
      (List.map (viewThumbnail model.selectedUrl) model.photos)
    , viewLarge model.selectedUrl
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SelectByUrl url ->
      ({ model | selectedUrl = Just url }, Cmd.none)
    SetSize size ->
      ({ model | chosenSize = size }, Cmd.none)
    SurpriseMe ->
      let
        photoPicker = Random.int 0 (List.length model.photos - 1)
      in
        (model, Random.generate SelectByIndex photoPicker)
    SelectByIndex index ->
      let
        newSelectedUrl : Maybe String
        newSelectedUrl =
          model.photos
            |> Array.fromList
            |> Array.get index
            |> Maybe.map .url
      in
        ({ model | selectedUrl = newSelectedUrl }, Cmd.none)

main : Program Never Model Msg
main =
  Html.program
    { init = (initialModel, Cmd.none)
    , view = view
    , update = update
    , subscriptions = (\model -> Sub.none)
    }
