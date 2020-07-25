module Page.Login exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Html exposing (Html, br, button, div, form, h1, img, input, label, p, text)
import Html.Attributes exposing (class, for, id, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Request exposing (UserResult(..), ResponseError(..))
import Session exposing (Session)



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
                UserSuccess { token, username, email } ->
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
        div []
            [ h1 [] [ text "Login here" ]
            , form [ onSubmit Submit ]
                [ credInput "text" "Login" model.login Login
                , credInput "password" "Password" model.password Password
                , br [] []
                , case model.state of
                    Waiting ->
                        button [ class "cred_submit", onClick Submit, type_ "submit" ] [ text "Submit" ]

                    Loading ->
                        button [ class "cred_submit", onClick Submit, type_ "submit" ] [ img [ src "/img/loading-horizontal.png" ] [] ]
                ]
            , case model.error of
                Just err ->
                    p [] [ text err ]

                Nothing ->
                    p [] []
            ]
    }


credInput : String -> String -> String -> (String -> Msg) -> Html Msg
credInput t p v a =
    div []
        [ label [ class "cred_label", for p ] [ text p ]
        , input [ class "cred_input", id p, type_ t, placeholder p, value v, onInput a ] []
        ]
