module Page.Login exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Element exposing (centerX, centerY, column, el, height, image, none, px, spacing, text, width)
import Element.Background as Background
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Request exposing (ResponseError(..), UserResult(..))
import Session exposing (Session)
import Style exposing (bgColor, buttonStyle, headingStyle, inputFieldStyle, textStyle)



-- MODEL


type alias Model =
    { session : Session
    , login : String
    , password : String
    , state : State
    , error : Maybe String
    }


type State
    = Waiting
    | Loading


init : Session -> Model
init session =
    Model session "" "" Waiting Nothing


type Msg
    = Login String
    | Password String
    | Submit
    | Response Request.UserResult



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login val ->
            ( { model | login = val }, Cmd.none )

        Password val ->
            ( { model | password = val }, Cmd.none )

        Submit ->
            ( { model | state = Loading }, Request.login model.login model.password Response )

        Response result ->
            case result of
                UserSuccess { token } ->
                    let
                        session =
                            model.session
                    in
                    ( { model | error = Nothing, state = Waiting, session = { session | token = Just token } }
                    , Navigation.pushUrl session.key "/"
                    )

                UserError UnauthorizedError ->
                    ( { model | error = Just "Login and password pair do not match", state = Waiting }
                    , Cmd.none
                    )

                UserError _ ->
                    ( { model | error = Just "Could not get reply from the server", state = Waiting }
                    , Cmd.none
                    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Login"
    , content =
        Element.layout (textStyle [ Background.color bgColor ]) <|
            column [ spacing 16, centerX, centerY, height (px 500), width (px 300) ]
                [ el (headingStyle [ Region.heading 1 ]) (text "Login here")
                , Input.username (inputFieldStyle [])
                    { onChange = Login
                    , text = model.login
                    , placeholder = Just <| Input.placeholder [] <| text "Login"
                    , label = Input.labelAbove [] <| text "Login:"
                    }
                , Input.currentPassword (inputFieldStyle [])
                    { onChange = Password
                    , text = model.password
                    , placeholder = Just <| Input.placeholder [] <| text "Password"
                    , label = Input.labelAbove [] <| text "Password:"
                    , show = False
                    }
                , case model.state of
                    Waiting ->
                        Input.button (buttonStyle [ centerX, height (px 50), width (px 150) ]) { onPress = Just Submit, label = el [ centerX ] <| text "Login" }

                    Loading ->
                        Input.button (buttonStyle [ centerX, height (px 50), width (px 150) ]) { onPress = Nothing, label = image [ height (px 20) ] { src = "/img/loading-horizontal.png", description = "loading..." } }
                , case model.error of
                    Just err ->
                        el [] (text err)

                    Nothing ->
                        el [] none
                ]
    }
