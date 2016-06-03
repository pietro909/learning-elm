import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (..)
import Regex exposing (..)

main =
    Html.beginnerProgram { model = model, update = update, view = view } 

-- MODEL

type alias Model =
    { name: String
    , password: String
    , passwordAgain: String
    }

model : Model
model =
    Model "" "" ""

-- UPDATE

type Msg
    = Name String
    | Password String
    | PasswordAgain String

update : Msg -> Model -> Model
update action model =
    case action of
        Name name ->
            { model | name = name }
        Password password ->
            { model | password = password }
        PasswordAgain password ->
            { model | passwordAgain = password }

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ input [ type' "text", placeholder "Name", onInput Name ] []
        , input [ type' "password", placeholder "Insert password", onInput Password ] []
        , input [ type' "password", placeholder "Confirm Passwor", onInput PasswordAgain ] []
        , viewValidation model
        ]

viewValidation : Model -> Html msg
viewValidation model =
    let
        (color, message) =
            if String.length model.password < 9 then
                ("red", "Password must be longer than 8 characters")
            else if not(Regex.contains (regex "([A-Z][a-z])|([a-z][A-Z])") model.password) then
                ("red", "Password must contain at least one uppercase and one lowercase character")
            else if model.password == model.passwordAgain then
                ("green", " OK")
            else
                ("red", "Password do not match!")
    in
       div [ style [("color", color)] ] [ text message ]

