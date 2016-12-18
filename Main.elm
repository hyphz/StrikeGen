module Main exposing (..)

import Html
import View exposing (..)
import CharModel exposing (..)
import ModelDB exposing (..)
import Ports exposing (..)


main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


subscriptions : Model -> Sub Msg
subscriptions _ =
    loadJson LoadJson
