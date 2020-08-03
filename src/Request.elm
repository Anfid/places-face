module Request exposing (FieldError, ResponseError(..), UserResponse, UserResult(..), login, register)

import Dict exposing (Dict)
import Http
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, list, map, map2, map3, oneOf, string, succeed)
import Json.Encode as Encode


backendUrl : String
backendUrl =
    "http://37.195.44.14:7878/"



-- GENERIC


type UserResult
    = UserSuccess UserResponse
    | UserError ResponseError


type alias UserResponse =
    { token : String
    , username : String
    , email : String
    }


type ResponseError
    = ValidationError (Dict String (List FieldError))
    | UnauthorizedError
    | AlreadyExistsError
    | NotFoundError
    | ServerError
    | NetworkError


type alias FieldError =
    { code : String
    , message : String
    }


userResponseDecoder : Decoder UserResult
userResponseDecoder =
    oneOf
        [ at [ "error", "kind" ] string |> andThen responseError |> map UserError
        , map UserSuccess <| map3 UserResponse (field "token" string) (field "username" string) (field "email" string)
        ]


responseError : String -> Decoder ResponseError
responseError kind =
    case kind of
        "field_validation" ->
            map ValidationError <| at [ "error", "info" ] <| dict <| list <| map2 FieldError (field "code" string) (field "message" string)

        "already_exists" ->
            succeed AlreadyExistsError

        "not_found" ->
            succeed NotFoundError

        "authorization" ->
            succeed UnauthorizedError

        _ ->
            fail "Unexpected error kind"


toUserResponse : Result Http.Error UserResult -> UserResult
toUserResponse res =
    case res of
        Ok val ->
            val

        Err (Http.BadStatus _) ->
            UserError ServerError

        Err (Http.BadUrl _) ->
            UserError ServerError

        Err (Http.BadBody _) ->
            UserError ServerError

        Err Http.Timeout ->
            UserError NetworkError

        Err Http.NetworkError ->
            UserError NetworkError



-- LOGIN


login : String -> String -> (UserResult -> msg) -> Cmd msg
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
        , expect = Http.expectJson (event << toUserResponse) userResponseDecoder
        }



-- REGISTER


register : String -> String -> String -> (UserResult -> msg) -> Cmd msg
register username email password event =
    let
        body =
            Encode.object
                [ ( "username", Encode.string username )
                , ( "email", Encode.string email )
                , ( "password", Encode.string password )
                ]
    in
    Http.post
        { url = backendUrl ++ "api/v1/users"
        , body = Http.jsonBody body
        , expect = Http.expectJson (event << toUserResponse) userResponseDecoder
        }
