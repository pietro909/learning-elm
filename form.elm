import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import String exposing (..)
import Regex exposing (..)
import Array exposing (..)

main =
    Html.beginnerProgram { model = model, update = update, view = view } 

-- MODEL

type alias Model =
    { name: String
    , age: Int
    , password: String
    , passwordAgain: String
    }

model : Model
model =
    Model "" 18 "" ""

-- UPDATE

type Msg
    = Name String
    | Age String
    | Password String
    | PasswordAgain String

update : Msg -> Model -> Model
update action model =
    case action of
        Age theAge ->
            { model | age = case Result.toMaybe(String.toInt theAge) of
                                Just v -> v
                                Nothing -> 0
            }
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
        , input [ type' "number", Html.Attributes.min "18", Html.Attributes.max "100", step "1", onInput Age ] []
        , viewValidation model
        , div[]
            [ button [ type' "submit", disabled(not(isValid model)) ] [ text "Register" ]
            ] 
        ]

findError : Model -> Int
findError model = 
    if model.age < 18 then
        0
    else if String.length model.password < 9 then
        1
    else if not(Regex.contains (regex "([A-Z][a-z])|([a-z][A-Z])") model.password) then
        2 
    else if model.password /= model.passwordAgain then
        3 
    else
        -1

isValid : Model -> Bool
isValid model = findError model < 0

errorMessages = Array.fromList
    [ "Age must be greater than 18"
    , "Password must be longer than 8 characters"
    , "Password must contain at least one uppercase and one lowercase character"
    , "Password do not match!"
    ]

viewValidation : Model -> Html msg
viewValidation model =
    let
        index = findError(model)
        mayHaveMessage = Array.get index errorMessages
        (color, message) =
            case mayHaveMessage of
                Just errMsg -> ("red", errMsg)
                Nothing -> ("green", "ok")
    in
       div [ style [("color", color)] ] [ text message ]

