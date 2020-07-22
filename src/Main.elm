module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html
import Url
import Url.Parser as Parser

import Page.Index
import Page.Register
import Page.Login



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { key : Navigation.Key
    , url : Url.Url
    , page : Page
    }


type Page
    = IndexPage Page.Index.Model
    | RegisterPage Page.Register.Model
    | LoginPage Page.Login.Model


init : flags -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url (IndexPage Page.Index.init), Cmd.none )



-- UPDATE


type Msg
    = IndexMsg Page.Index.Msg
    | RegisterMsg Page.Register.Msg
    | LoginMsg Page.Login.Msg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Navigation.load href )

        ( UrlChanged url, _ ) ->
            gotoUrl url model

        ( IndexMsg indexMsg, IndexPage indexModel ) ->
            Page.Index.update indexMsg indexModel
                |> updateWith IndexPage IndexMsg model

        ( RegisterMsg regMsg, RegisterPage regModel ) ->
            Page.Register.update regMsg regModel
                |> updateWith RegisterPage RegisterMsg model

        ( LoginMsg loginMsg, LoginPage loginModel ) ->
            Page.Login.update loginMsg loginModel
                |> updateWith LoginPage LoginMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Page) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toPage toMsg model ( subModel, subCmd ) =
    ( { model | page = toPage subModel }
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        transformMsg pageView toMsg =
            let
                { title, content } =
                    pageView
            in
            { title = title
            , body = [ Html.map toMsg content ]
            }
    in
    case model.page of
        IndexPage index ->
            transformMsg (Page.Index.view index) IndexMsg

        RegisterPage reg ->
            transformMsg (Page.Register.view reg) RegisterMsg

        LoginPage login ->
            transformMsg (Page.Login.view login) LoginMsg



-- ROUTER


type Route
    = IndexRoute
    | RegisterRoute
    | LoginRoute


parser: Parser.Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map IndexRoute Parser.top
        , Parser.map LoginRoute (Parser.s "login")
        , Parser.map RegisterRoute (Parser.s "register")
        ]


gotoUrl : Url.Url -> Model -> ( Model, Cmd Msg )
gotoUrl url model =
    case Parser.parse parser url of
        Just IndexRoute ->
            ( { model | page = IndexPage Page.Index.init, url = url }, Cmd.none )

        Just RegisterRoute ->
            ( { model | page = RegisterPage Page.Register.init, url = url }, Cmd.none )

        Just LoginRoute ->
            ( { model | page = LoginPage Page.Login.init, url = url }, Cmd.none )

        Nothing ->
            ( model, Cmd.none )
