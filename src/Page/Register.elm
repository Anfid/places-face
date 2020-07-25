module Page.Register exposing (Model, Msg, init, update, view)

import Html exposing (Html, br, button, div, form, h1, input, label, text)
import Html.Attributes exposing (class, for, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Session exposing (Session)


type alias Model =
    { session : Session
    , login : InputField
    , password : InputField
    , passwordAgain : InputField
    , state: State
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


type FieldError
    = Empty
    | BadChar
    | BadLen


type State
    = Waiting
    | Loading

init : Session -> Model
init session =
    Model session (InputField "" <| Just Empty) (InputField "" <| Just Empty) (InputField "" <| Just Empty) Waiting


type Msg
    = Login String
    | Password String
    | PasswordAgain String
    | Submit



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login val ->
            ( { model | login = updateFieldContent model.login val }, Cmd.none )

        Password val ->
            ( { model | password = updateFieldContent model.password val }, Cmd.none )

        PasswordAgain val ->
            ( { model | passwordAgain = updateFieldContent model.passwordAgain val }, Cmd.none )

        Submit ->
            ( model, Cmd.none )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Register"
    , content =
        div []
            [ form [ onSubmit Submit ]
                [ h1 [] [ text "Register here" ]
                , credInput "text" "Login" model.login.content Login
                , credInput "password" "Password" model.password.content Password
                , credInput "password" "Re-enter Password" model.passwordAgain.content PasswordAgain
                , br [] []
                , button [ class "cred_submit" ] [ text "Submit" ]
                ]
            ]
    }


credInput : String -> String -> String -> (String -> Msg) -> Html Msg
credInput t p v a =
    div []
        [ label [ class "cred_label", for p ] [ text p ]
        , input [ class "cred_input", id p, type_ t, placeholder p, value v, onInput a ] []
        ]
