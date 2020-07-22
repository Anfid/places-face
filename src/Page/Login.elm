module Page.Login exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


type alias Model =
    { login : String
    , password : String
    }


init: Model
init =
    Model "" ""


type Msg
    = Login String
    | Password String



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login val ->
            ( { model | login = val }, Cmd.none )

        Password val ->
            ( { model | password = val }, Cmd.none )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Login"
    , content =
        div []
            [ h1 [] [ text "Login here" ]
            , input [ type_ "text", placeholder "Name", value model.login, onInput Login ] []
            , input [ type_ "password", placeholder "Password", value model.password, onInput Password ] []
            ]
    }
