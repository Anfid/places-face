module Page.Register exposing (Model, Msg, init, update, view)

import Browser.Navigation as Navigation
import Dict exposing (toList)
import Element exposing (centerX, centerY, column, el, height, none, px, spacing, text, width)
import Element.Background as Background
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Request exposing (FieldError, ResponseError(..), UserResult(..))
import Session exposing (Session)
import Style exposing (bgColor, buttonStyle, headingStyle, inputFieldStyle, textStyle)


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


updateFieldContent : String -> InputField -> InputField
updateFieldContent content field =
    { field | content = content }


updateFieldError : Maybe FieldError -> InputField -> InputField
updateFieldError err field =
    { field | error = err }


resetFieldErrors : Model -> Model
resetFieldErrors model =
    { model
        | login = updateFieldError Nothing model.login
        , email = updateFieldError Nothing model.email
        , password = updateFieldError Nothing model.password
        , passwordAgain = updateFieldError Nothing model.passwordAgain
    }


type State
    = Waiting
    | Loading


init : Session -> Model
init session =
    Model session (InputField "" Nothing) (InputField "" Nothing) (InputField "" Nothing) (InputField "" Nothing) Waiting


type Msg
    = Login String
    | Email String
    | Password String
    | PasswordAgain String
    | ValidateFields
    | Submit
    | Response Request.UserResult



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login val ->
            ( { model | login = updateFieldContent val model.login }, Cmd.none )

        Email val ->
            ( { model | email = updateFieldContent val model.email }, Cmd.none )

        Password val ->
            let
                mod =
                    { model | password = updateFieldContent val model.password }
            in
            ( { mod | passwordAgain = validatePasswordAgain mod.password mod.passwordAgain }
            , Cmd.none
            )

        PasswordAgain val ->
            ( { model | passwordAgain = validatePasswordAgain model.password <| updateFieldContent val model.passwordAgain }
            , Cmd.none
            )

        ValidateFields ->
            ( validateFields model, Cmd.none )

        Submit ->
            ( resetFieldErrors { model | state = Loading }
            , Request.register model.login.content model.email.content model.password.content Response
            )

        Response resp ->
            handleResponse resp model


validateFields : Model -> Model
validateFields model =
    { model
        | login = validateLogin model.login
        , email = validateEmail model.email
        , password = validatePassword model.password
        , passwordAgain = validatePasswordAgain model.password model.passwordAgain
    }


validateLogin : InputField -> InputField
validateLogin login =
    if String.isEmpty login.content then
        login

    else if String.length login.content > 20 then
        InputField login.content <|
            Just <|
                FieldError "len" "Username length is expected to be between 1 and 20 characters"

    else
        updateFieldError Nothing login


validateEmail : InputField -> InputField
validateEmail email =
    let
        err =
            case String.split "@" email.content of
                [ "" ] ->
                    email.error

                [ user, domain ] ->
                    if not <| String.isEmpty user || String.isEmpty domain then
                        Nothing

                    else
                        Just <| FieldError "email" "Invalid email"

                _ ->
                    Just <| FieldError "email" "Invalid email"
    in
    updateFieldError err email


validatePassword : InputField -> InputField
validatePassword pass =
    if String.isEmpty pass.content then
        pass

    else if String.length pass.content < 8 || String.length pass.content > 128 then
        InputField pass.content <|
            Just <|
                FieldError "len" "Pass length is expected to be between 8 and 128 characters"

    else
        updateFieldError Nothing pass


validatePasswordAgain : InputField -> InputField -> InputField
validatePasswordAgain pass pass2 =
    if pass.content /= pass2.content && not (String.isEmpty pass2.content) then
        InputField pass2.content <|
            Just <|
                FieldError "repeat" "Passwords do not match"

    else
        updateFieldError Nothing pass2


handleResponse : Request.UserResult -> Model -> ( Model, Cmd Msg )
handleResponse res model =
    case res of
        UserSuccess { token } ->
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
                            { model | login = updateFieldError (Just fe) model.login }

                        ( "email", fe :: _ ) ->
                            { model | email = updateFieldError (Just fe) model.email }

                        ( "password", fe :: _ ) ->
                            { model | password = updateFieldError (Just fe) model.password }

                        _ ->
                            model
            in
            handleValidationError mod <| tail



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Register"
    , content =
        Element.layout (textStyle [ Background.color bgColor ]) <|
            column
            [ spacing 16, centerX, centerY, height (px 500), width (px 300) ]
            [ el (headingStyle [ Region.heading 1 ]) (text "Register here")
            , Input.username (inputFieldStyle [])
                { onChange = Login
                , text = model.login.content
                , placeholder = Just <| Input.placeholder [] <| text "Login"
                , label = Input.labelAbove [] <| text "Login:"
                }
            , Input.text (inputFieldStyle [])
                { onChange = Email
                , text = model.email.content
                , placeholder = Just <| Input.placeholder [] <| text "E-mail"
                , label = Input.labelAbove [] <| text "E-mail:"
                }
            , Input.newPassword (inputFieldStyle [])
                { onChange = Password
                , text = model.password.content
                , placeholder = Just <| Input.placeholder [] <| text "Password"
                , label = Input.labelAbove [] <| text "Password:"
                , show = False
                }
            , Input.newPassword (inputFieldStyle [])
                { onChange = PasswordAgain
                , text = model.passwordAgain.content
                , placeholder = Just <| Input.placeholder [] <| text "Re-enter Password"
                , label = Input.labelAbove [] <| text "Re-enter Password:"
                , show = False
                }
            , Input.button (buttonStyle [ centerX, height (px 50), width (px 150) ]) { onPress = Just Submit, label = el [ centerX ] <| text "Register" }
            ]
    }
