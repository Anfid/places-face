module Page.Register exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Dict exposing (toList)
import Html exposing (Html, br, button, div, form, h1, input, label, text)
import Html.Attributes exposing (class, for, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Request exposing (FieldError, ResponseError(..), UserResult(..))
import Session exposing (Session)


type alias Model =
    { session : Session
    , login : InputField
    , email : InputField
    , password : InputField
    , passwordAgain : InputField
    , state : State
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


type State
    = Waiting
    | Loading


init : Session -> Model
init session =
    Model session (InputField "" <| Nothing) (InputField "" <| Nothing) (InputField "" <| Nothing) (InputField "" <| Nothing) Waiting


type Msg
    = Login String
    | Email String
    | Password String
    | PasswordAgain String
    | Submit
    | Response Request.UserResult



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login val ->
            ( { model | login = updateFieldContent model.login val }, Cmd.none )

        Email val ->
            ( { model | email = updateFieldContent model.email val }, Cmd.none )

        Password val ->
            ( { model | password = updateFieldContent model.password val }, Cmd.none )

        PasswordAgain val ->
            ( { model | passwordAgain = updateFieldContent model.passwordAgain val }, Cmd.none )

        Submit ->
            ( { model | state = Loading }
            , Request.register model.login.content model.email.content model.password.content Response
            )

        Response resp ->
            handleResponse resp model


handleResponse : Request.UserResult -> Model -> ( Model, Cmd Msg )
handleResponse res model =
    case res of
        UserSuccess { token, username, email } ->
            let
                session =
                    model.session
            in
            ( { model | state = Waiting, session = { session | token = Just token } }
            , Navigation.pushUrl session.key "/"
            )

        UserError (ValidationError e) ->
            ( handleValidationError model <| toList e, Cmd.none )

        _ ->
            ( model, Cmd.none )


handleValidationError : Model -> List ( String, List FieldError ) -> Model
handleValidationError model errs =
    case errs of
        [] ->
            model

        e :: tail ->
            let
                mod =
                    case e of
                        ( "username", fe :: _ ) ->
                            { model | login = updateFieldError model.login <| Just fe }

                        ( "email", fe :: _ ) ->
                            { model | email = updateFieldError model.email <| Just fe }

                        ( "password", fe :: _ ) ->
                            { model | password = updateFieldError model.password <| Just fe }

                        _ ->
                            model
            in
            handleValidationError mod <| tail



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Register"
    , content =
        div []
            [ form [ onSubmit Submit ]
                [ h1 [] [ text "Register here" ]
                , credInput "text" "Login" model.login Login
                , credInput "text" "E-mail" model.email Email
                , credInput "password" "Password" model.password Password
                , credInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
                , br [] []
                , button [ class "cred_submit" ] [ text "Submit" ]
                ]
            ]
    }


credInput : String -> String -> InputField -> (String -> Msg) -> Html Msg
credInput t ph field action =
    div []
        [ label [ class "cred_label", for ph ] [ text ph ]
        , case field.error of
            Just { code, message } ->
                input [ class "cred_input err", id ph, type_ t, placeholder ph, value field.content, onInput action ] []
            Nothing ->
                input [ class "cred_input", id ph, type_ t, placeholder ph, value field.content, onInput action ] []
        ]
