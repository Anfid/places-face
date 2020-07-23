module Page.Register exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


type alias Model =
    { login : InputField
    , password : InputField
    , passwordAgain : InputField
    }


type alias InputField =
    { content : String
    , error : Maybe FieldError
    }


updateFieldContent : InputField -> String -> InputField
updateFieldContent field content =
    { field | content = content }


updateFieldError : InputField -> Maybe FieldError -> InputField
updateFieldError field err =
    { field | error = err }


type FieldError
    = Empty
    | BadChar
    | BadLen



init: Model
init =
    Model (InputField "" <| Just Empty) (InputField "" <| Just Empty) (InputField "" <| Just Empty)


type Msg
    = Login String
    | Password String
    | PasswordAgain String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login val ->
            ( { model | login = updateFieldContent model.login val }, Cmd.none )

        Password val ->
            ( { model | password = updateFieldContent model.password val }, Cmd.none )

        PasswordAgain val ->
            ( { model | passwordAgain = updateFieldContent model.passwordAgain val }, Cmd.none )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Register"
    , content =
        div []
            [ h1 [] [ text "Register here" ]
            , input [ type_ "text", placeholder "Name", value model.login.content, onInput Login ] []
            , br [] []
            , input [ type_ "password", placeholder "Password", value model.password.content, onInput Password ] []
            , br [] []
            , input [ type_ "password", placeholder "Re-enter Password", value model.passwordAgain.content, onInput PasswordAgain ] []
            , br [] []
            , br [] []
            , button [] [ text "Submit" ]
            ]
    }
