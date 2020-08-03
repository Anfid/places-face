module Page.Index exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Element exposing (alignRight, padding, row, spacing, text)
import Element.Background as Background
import Element.Input exposing (button)
import Html exposing (Html)
import Session exposing (Session)
import Style exposing (bgColor, buttonStyle)


type alias Model =
    { session : Session
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
        Element.layout [ Background.color bgColor ] <|
            row [ alignRight, spacing 5, padding 5 ]
                [ button (buttonStyle [])
                    { onPress = Just <| GotoLink "/login", label = text "Login" }
                , button (buttonStyle [])
                    { onPress = Just <| GotoLink "/register", label = text "Register" }
                ]
    }
