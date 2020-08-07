module Component.Login exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Element exposing (Element, centerX, centerY, column, el, height, image, none, px, spacing, text, width)
import Element.Input as Input
import Element.Region as Region
import Html.Events
import Json.Decode as Decode
import PageMsg exposing (PageMsg)
import Request exposing (ResponseError(..), UserResult(..))
import Session exposing (Session)
import Style exposing (buttonStyle, headingStyle, inputFieldStyle)



-- MODEL


type alias Model =
    { login : String
    , password : String
    , state : State
    , error : Maybe String
    }


type State
    = Waiting
    | Loading


init : Model
init =
    Model "" "" Waiting Nothing



-- UPDATE


type Msg
    = Login String
    | Password String
    | Submit
    | Response Request.UserResult


update : Msg -> Session -> Model -> ( Model, PageMsg, Cmd Msg )
update msg session model =
    case msg of
        Login val ->
            ( { model | login = val }, PageMsg.None, Cmd.none )

        Password val ->
            ( { model | password = val }, PageMsg.None, Cmd.none )

        Submit ->
            ( { model | state = Loading }, PageMsg.None, Request.login model.login model.password Response )

        Response result ->
            case result of
                UserSuccess { token } ->
                    ( { model | error = Nothing, state = Waiting }
                    , PageMsg.Login token
                    , Navigation.pushUrl session.key "/"
                    )

                UserError UnauthorizedError ->
                    ( { model | error = Just "Login and password pair do not match", state = Waiting }
                    , PageMsg.None
                    , Cmd.none
                    )

                UserError _ ->
                    ( { model | error = Just "Could not get reply from the server", state = Waiting }
                    , PageMsg.None
                    , Cmd.none
                    )



-- VIEW


view : Model -> Element Msg
view model =
    column [ spacing 16, centerX, centerY, height (px 500), width (px 300) ]
        [ el (headingStyle [ Region.heading 1 ]) (text "Login here")
        , Input.username (inputFieldStyle [ onEnter Submit ])
            { onChange = Login
            , text = model.login
            , placeholder = Just <| Input.placeholder [] <| text "Login"
            , label = Input.labelAbove [] <| text "Login:"
            }
        , Input.currentPassword (inputFieldStyle [ onEnter Submit ])
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


onEnter : msg -> Element.Attribute msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )
