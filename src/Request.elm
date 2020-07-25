module Request exposing (FieldError, LoginResult(..), LoginSuccessReply, ReplyError(..), login)

import Dict exposing (Dict)
import Http
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, list, map, map2, map3, oneOf, string, succeed)
import Json.Encode as Encode


backendUrl : String
backendUrl =
    "http://37.195.44.14:7878/"



-- ERRORS


type ReplyError
    = ValidationError (Dict String (List FieldError))
    | UnauthorizedError
    | NotFoundError
    | ServerError
    | NetworkError


type alias FieldError =
    { code : String
    , message : String
    }


replyError : String -> Decoder ReplyError
replyError kind =
    case kind of
        "field_validation" ->
            map ValidationError <| at [ "error", "info" ] <| dict <| list <| map2 FieldError (field "code" string) (field "message" string)

        "not_found" ->
            succeed NotFoundError

        "authorization" ->
            succeed UnauthorizedError

        _ ->
            fail "Unexpected error kind"



-- LOGIN


type LoginResult
    = LoginSuccess LoginSuccessReply
    | LoginError ReplyError


type alias LoginSuccessReply =
    { token : String
    , username : String
    , email : String
    }


login : String -> String -> (LoginResult -> msg) -> Cmd msg
login username password event =
    let
        body =
            Encode.object
                [ ( "username", Encode.string username )
                , ( "password", Encode.string password )
                ]
    in
    Http.post
        { url = backendUrl ++ "api/v1/users/login"
        , body = Http.jsonBody body
        , expect = Http.expectJson (event << toLoginReply) loginReplyDecoder
        }


toLoginReply : Result Http.Error LoginResult -> LoginResult
toLoginReply res =
    case res of
        Ok val ->
            val

        Err (Http.BadStatus _) ->
            LoginError ServerError

        Err (Http.BadUrl _) ->
            LoginError ServerError

        Err (Http.BadBody _) ->
            LoginError ServerError

        Err Http.Timeout ->
            LoginError NetworkError

        Err Http.NetworkError ->
            LoginError NetworkError


loginReplyDecoder : Decoder LoginResult
loginReplyDecoder =
    oneOf
        [ at [ "error", "kind" ] string |> andThen replyError |> map LoginError
        , map LoginSuccess <| map3 LoginSuccessReply (field "token" string) (field "username" string) (field "email" string)
        ]
