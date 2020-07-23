module Page.Login exposing (Model, Msg, init, update, view)

import Dict exposing (Dict)
import Html exposing (Html, br, button, div, form, h1, input, p, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, list, map, map2, oneOf, string, succeed)
import Json.Encode



-- MODEL


type alias Model =
    { login : String
    , password : String
    , error : Maybe String
    }


init : Model
init =
    Model "" "" Nothing


type Msg
    = Login String
    | Password String
    | Submit
    | Reply (Result Http.Error LoginReply)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Login val ->
            ( { model | login = val }, Cmd.none )

        Password val ->
            ( { model | password = val }, Cmd.none )

        Submit ->
            submit model

        Reply result ->
            case result of
                Ok (Success val) ->
                    ( { model | error = Just val }, Cmd.none )

                Ok (Error (ValidationError e)) ->
                    ( { model | error = if Dict.member "username" e then Just "Invalid username" else Nothing }, Cmd.none )

                Ok (Error NotFoundError) ->
                    ( { model | error = Just "User does not exist" }, Cmd.none )

                Err e ->
                    let
                        errtxt =
                            case e of
                                Http.BadUrl s ->
                                    "BadUrl: " ++ s

                                Http.Timeout ->
                                    "Timeout"

                                Http.NetworkError ->
                                    "NetworkError"

                                Http.BadStatus s ->
                                    "BadStatus: " ++ String.fromInt s

                                Http.BadBody s ->
                                    "BadBody: " ++ s
                    in
                    ( { model | error = Just errtxt }, Cmd.none )


submit : Model -> ( Model, Cmd Msg )
submit model =
    let
        json =
            Json.Encode.object
                [ ( "username", Json.Encode.string model.login )
                , ( "password", Json.Encode.string model.password )
                ]
    in
    ( model
    , Http.post
        { url = "http://192.168.0.188:7878/api/v1/users/login"
        , body = Http.jsonBody json
        , expect = Http.expectJson Reply loginReplyDecoder
        }
    )


type LoginReply
    = Success String
    | Error ReplyError


type ReplyError
    = ValidationError (Dict String (List FieldError))
    | NotFoundError


type alias FieldError =
    { code : String
    , message : String
    }


loginReplyDecoder : Decoder LoginReply
loginReplyDecoder =
    oneOf
        [ at [ "error", "kind" ] string |> andThen replyError |> map Error
        , field "token" string |> map Success
        ]


replyError : String -> Decoder ReplyError
replyError kind =
    case kind of
        "field_validation" ->
            map ValidationError <| at [ "error", "info" ] <| dict <| list <| map2 FieldError (field "code" string) (field "message" string)

        "not_found" ->
            succeed NotFoundError

        _ ->
            fail "Unexpected error kind"



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Login"
    , content =
        div []
            [ h1 [] [ text "Login here" ]
            , form [ onSubmit Submit ]
                [ credInput "text" "Login" model.login Login
                , br [] []
                , credInput "password" "Password" model.password Password
                , br [] []
                , br [] []
                , button [ onClick Submit, type_ "submit" ] [ text "Submit" ]
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
    input [ type_ t, placeholder p, value v, onInput a ] []
