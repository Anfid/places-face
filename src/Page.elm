module Page exposing (Msg(..), init, update, view, Model)

import Browser
import Browser.Navigation as Navigation
import Html
import Page.Index
import PageMsg exposing (PageMsg)
import Session exposing (Session)
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser)



-- MODEL


init : flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    gotoUrl url <| Model (Session key Nothing) url <| Index <| Page.Index.init


type alias Model =
    { session : Session
    , url : Url
    , page : Page
    }


type Page
    = Index Page.Index.Model



-- UPDATE


type Msg
    = UrlChanged Url
    | LinkClicked Browser.UrlRequest
    | IndexMsg Page.Index.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( UrlChanged url, _ ) ->
            gotoUrl url model

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.session.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        ( IndexMsg subMsg, Index subModel ) ->
            Page.Index.update subMsg model.session subModel
                |> handleUpdate Index IndexMsg model


handleUpdate : (subModel -> Page) -> (subMsg -> Msg) -> Model -> ( subModel, PageMsg, Cmd subMsg ) -> ( Model, Cmd Msg )
handleUpdate toPage toMsg model ( subModel, pageMsg, subCmd ) =
    ( { model | page = toPage subModel }
    , Cmd.map toMsg subCmd
    )


type Route
    = IndexRoute
    | RegisterRoute
    | LoginRoute


parser : Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map IndexRoute Parser.top
        ]


gotoUrl : Url -> Model -> ( Model, Cmd Msg )
gotoUrl url model =
    case Parser.parse parser url of
        Just IndexRoute ->
            ( { model | page = Index <| Page.Index.init, url = url }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model.page of
        Index subModel ->
            { title = "Home", body = [ Html.map IndexMsg <| Page.Index.view subModel ] }
