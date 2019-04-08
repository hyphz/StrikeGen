module Main exposing (main)

import Html
import Browser
import View
import ModelDB
import CharModel
import Ports

main : Program Int ModelDB.Model ModelDB.Msg
main =
    Browser.element { init = CharModel.init, view = View.view, update = CharModel.update, subscriptions = subscriptions }


subscriptions : ModelDB.Model -> Sub ModelDB.Msg
subscriptions _ =
    Ports.loadJson ModelDB.LoadJson
