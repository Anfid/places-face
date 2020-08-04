module Page.Index exposing (Model, Msg, init, update, view)

import Component.Bar as BarComponent
import Component.Login as LoginComponent
import Component.Register as RegisterComponent
import Element exposing (fill, none, width)
import Element.Background as Background
import Html exposing (Html)
import PageMsg exposing (PageMsg)
import Session exposing (Session)
import Style exposing (bgColor)



-- MODEL


type alias Model =
    { bar : BarComponent.Model
    , signinup : SignInUp
    }


type SignInUp
    = Nothing
    | Login LoginComponent.Model
    | Register RegisterComponent.Model


init : Model
init =
    { bar = BarComponent.init
    , signinup = Nothing
    }



-- UPDATE


type Msg
    = BarMsg BarComponent.Msg
    | LoginMsg LoginComponent.Msg
    | RegisterMsg RegisterComponent.Msg


update : Msg -> Session -> Model -> ( Model, PageMsg, Cmd Msg )
update msg session model =
    case msg of
        BarMsg m ->
            handleUpdate updateBarModel BarMsg model <| BarComponent.update m session model.bar

        LoginMsg m ->
            case model.signinup of
                Login loginModel ->
                    handleUpdate updateLoginModel LoginMsg model <| LoginComponent.update m session loginModel

                _ ->
                    ( model, PageMsg.None, Cmd.none )

        RegisterMsg m ->
            case model.signinup of
                Register registerModel ->
                    handleUpdate updateRegisterModel RegisterMsg model <| RegisterComponent.update m session registerModel

                _ ->
                    ( model, PageMsg.None, Cmd.none )


handleUpdate :
    (subModel -> Model -> Model)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, PageMsg, Cmd subMsg )
    -> ( Model, PageMsg, Cmd Msg )
handleUpdate toModel toMsg model ( subModel, pageMsg, subCmd ) =
    let
        newModel =
            case pageMsg of
                PageMsg.ShowLogin ->
                    { model | signinup = Login LoginComponent.init }

                PageMsg.ShowRegister ->
                    { model | signinup = Register RegisterComponent.init }

                _ ->
                    model
    in
    ( toModel subModel newModel, pageMsg, Cmd.map toMsg subCmd )


updateBarModel : BarComponent.Model -> Model -> Model
updateBarModel barModel model =
    { model | bar = barModel }


updateLoginModel : LoginComponent.Model -> Model -> Model
updateLoginModel loginModel model =
    { model | signinup = Login loginModel }


updateRegisterModel : RegisterComponent.Model -> Model -> Model
updateRegisterModel registerModel model =
    { model | signinup = Register registerModel }



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout [ Background.color bgColor ] <|
        Element.column [ width fill ]
            [ Element.map BarMsg <|
                BarComponent.view model.bar
            , case model.signinup of
                Login loginModel ->
                    Element.map LoginMsg <|
                        LoginComponent.view loginModel

                Register registerModel ->
                    Element.map RegisterMsg <|
                        RegisterComponent.view registerModel

                Nothing ->
                    none
            ]
