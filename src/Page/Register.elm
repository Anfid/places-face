module Page.Register exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


type alias Model =
    { login : String
    , password : String
    , passwordAgain : String
    }


init: Model
init =
    Model "" "" ""


type Msg
    = Login String
    | Password String
    | PasswordAgain String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login val ->
            ( { model | login = val }, Cmd.none )

        Password val ->
            ( { model | password = val }, Cmd.none )

        PasswordAgain val ->
            ( { model | passwordAgain = val }, Cmd.none )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Register"
    , content =
        div []
            [ h1 [] [ text "Register here" ]
            , input [ type_ "text", placeholder "Name", value model.login, onInput Login ] []
            , br [] []
            , input [ type_ "password", placeholder "Password", value model.password, onInput Password ] []
            , br [] []
            , input [ type_ "password", placeholder "Re-enter Password", value model.passwordAgain, onInput PasswordAgain ] []
            , br [] []
            , br [] []
            , button [] [ text "Submit" ]
            ]
    }
