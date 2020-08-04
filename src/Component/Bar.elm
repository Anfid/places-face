module Component.Bar exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Element exposing (Element, alignRight, padding, row, spacing, text)
import Element.Input exposing (button)
import PageMsg exposing (PageMsg)
import Session exposing (Session)
import Style exposing (buttonStyle)


init : Model
init = ()

type Msg
    = ShowLogin
    | ShowRegister


type alias Model =
    ()


update : Msg -> Session -> Model -> ( Model, PageMsg, Cmd Msg )
update msg session model =
    case msg of
        ShowLogin ->
            ( model, PageMsg.ShowLogin, Navigation.pushUrl session.key "/login" )

        ShowRegister ->
            ( model, PageMsg.ShowRegister, Navigation.pushUrl session.key "/register" )


view : Model -> Element Msg
view _ =
    row [ alignRight, spacing 5, padding 5 ]
        [ button (buttonStyle [])
            { onPress = Just ShowLogin, label = text "Login" }
        , button (buttonStyle [])
            { onPress = Just ShowRegister, label = text "Register" }
        ]
