port module PhotoGroove exposing (..)

import Html exposing (..)
import Html.Attributes as Attr exposing (id, class, classList, src,  name, type_, title, checked)
import Html.Events exposing (on, onClick)
import Array exposing (Array)
import Random
import Http
import Json.Decode exposing (Decoder, at, int, list, string)
import Json.Decode.Pipeline exposing (decode, required, optional)

paperSlider : List (Attribute msg) -> List (Html msg) -> Html msg
paperSlider = Html.node "paper-slider"

viewFilter : String -> (Int -> msg) -> Int -> Html msg
viewFilter name toMsg magnitude =
  div [ class "filter-slider" ]
    [ label [] [ text name ]
    , paperSlider [ Attr.max "11", onImmediateValueChange toMsg ] []
    , label [] [ text (toString magnitude) ]
    ]

onImmediateValueChange : (Int -> msg) -> Attribute msg
onImmediateValueChange toMsg =
  at [ "target", "immediateValue" ] int
    |> Json.Decode.map toMsg
    |> on "immediate-value-changed"

type alias Photo =
  { size : Int
  , title : String
  , url : String
  }

port setFilters : FilterOptions -> Cmd msg

type alias FilterOptions =
  { url : String
  , filters : List { name : String, amount : Float }
  }

photoDecoder : Decoder Photo
photoDecoder =
  decode Photo
    |> required "size" int
    |> optional "title" string "(untitled)"
    |> required "url" string

type ThumbnailSize
  = Small
  | Medium
  | Large

type alias Model =
  { photos : List Photo
  , selectedUrl : Maybe String
  , loadingError : Maybe String
  , chosenSize : ThumbnailSize
  , hue : Int
  , ripple : Int
  , noise : Int
  }

initialModel : Model
initialModel =
  { photos = [ ]
  , selectedUrl = Nothing
  , loadingError = Nothing
  , chosenSize = Medium
  , hue = 0
  , ripple = 0
  , noise = 0
  }

type Msg
  = SelectByUrl String
  | SelectByIndex Int
  | SetSize ThumbnailSize
  | SurpriseMe
  | LoadPhotos (Result Http.Error (List Photo))
  | SetHue Int
  | SetRipple Int
  | SetNoise Int

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
    , title (thumbnail.title ++ " [" ++ toString thumbnail.size ++ "KB]")
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
      canvas [ id "main-canvas", class "large" ] []

viewOrError : Model -> Html Msg
viewOrError model =
  case model.loadingError of
    Nothing -> view model
    Just errorMessage ->
      div [ class "error-message" ]
        [ h1 [] [ text "Photo Groove" ]
        , p [] [ text errorMessage ]
        ]

view : Model -> Html Msg
view model =
  div [ class "content" ]
    [ h1 [] [ text "Photo Groove" ]
    , button
      [ onClick SurpriseMe ]
      [ text "Surprise me!" ]
    , div [ class "filters" ]
      [ viewFilter "Hue" SetHue model.hue
      , viewFilter "Ripple" SetRipple model.ripple
      , viewFilter "Noise" SetNoise model.noise
      ]
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
      let
        newModel = { model | selectedUrl = Just url }
        cmd = applyFilters newModel
      in
        (newModel, cmd)
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
        newModel = { model | selectedUrl = newSelectedUrl }
        cmd = applyFilters newModel
      in
        (newModel, cmd)
    LoadPhotos (Ok photos) ->
      let
        newModel =
          { model | photos = photos
          , selectedUrl = Maybe.map .url (List.head photos)
          }
        cmd = applyFilters newModel
      in
        (newModel, cmd)
    LoadPhotos (Err _) ->
      ( { model
          | loadingError = Just  "There was an error with HTTP"
        }
      , Cmd.none
      )
    SetHue amount ->
      let
        newModel = { model | hue = amount }
        cmd = applyFilters newModel
      in
        (newModel, cmd)
    SetRipple amount ->
      let
        newModel = { model | ripple = amount }
        cmd = applyFilters newModel
      in
        (newModel, cmd)
    SetNoise amount ->
      let
        newModel = { model | noise = amount }
        cmd = applyFilters newModel
      in
        (newModel, cmd)

applyFilters : Model -> Cmd Msg
applyFilters model =
  case model.selectedUrl of
    Just selectedUrl ->
      let
        filters =
          [ { name = "Hue", amount = (toFloat model.hue) / 11 }
          , { name = "Ripple", amount = (toFloat model.ripple) / 11 }
          , { name = "Noise", amount = (toFloat model.noise) / 11 }
          ]
        url = urlPrefix ++ "large/" ++ selectedUrl
      in
        setFilters { url = url, filters = filters }
    Nothing ->
      Cmd.none

initialCmd : Cmd Msg
initialCmd =
  list photoDecoder 
    |> Http.get "http://elm-in-action.com/photos/list.json"
    |> Http.send LoadPhotos

-- Http.send LoadPhotos (Http.get "http://elm-in-action.com/photos/list.json"  (list photoDecoder) )

main : Program Never Model Msg
main =
  Html.program
    { init = (initialModel, initialCmd)
    , view = viewOrError
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }
