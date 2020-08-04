module Main exposing (main)

import Browser
import Browser.Events as Events
import Browser.Navigation as Navigation
import Element exposing (Device, classifyDevice)
import Html
import Page
import Page.Index
import Session exposing (Session)
import Url
import Url.Parser as Parser



-- MAIN


main : Program () Page.Model Page.Msg
main =
    Browser.application
        { init = Page.init
        , update = Page.update
        , view = Page.view
        , onUrlChange = Page.UrlChanged
        , onUrlRequest = Page.LinkClicked
        , subscriptions = subscriptions
        }


-- SUBSCRIPTIONS


subscriptions : Page.Model -> Sub Page.Msg
subscriptions _ =
    Sub.none
    --Events.onResize (\w h -> classifyDevice { width = w, height = h }) |> Sub.map
