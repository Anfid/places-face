module Page.Index exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (href)


type alias Model =
    {  }


init =
    Model


type Msg
    = Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nothing ->
            ( model, Cmd.none )


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Index"
    , content =
        div []
            [ a [ href "/login" ] [ button [] [ text "Login" ] ]
            , br [] []
            , a [ href "/register" ] [ button [] [ text "Register" ] ]
            ]
    }
