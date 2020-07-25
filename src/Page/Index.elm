module Page.Index exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Session exposing (Session)


type alias Model =
    { session: Session
    }


init : Session -> Model
init session =
    Model session


type Msg
    = GotoLink String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotoLink url ->
            ( model, Navigation.pushUrl model.session.key url )


view : Model -> { title : String, content : Html Msg }
view _ =
    { title = "Index"
    , content =
        div []
            [ button [ onClick <| GotoLink "/login" ] [ text "Login" ]
            , button [ onClick <| GotoLink "/register" ] [ text "Register" ]
            ]
    }
