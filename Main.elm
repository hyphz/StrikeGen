module Main exposing (main)

import Browser
import CharModel
import Html
import ModelDB
import Ports
import View


main : Program Int ModelDB.Model ModelDB.Msg
main =
    Browser.element { init = CharModel.init, view = View.view, update = CharModel.update, subscriptions = subscriptions }


subscriptions : ModelDB.Model -> Sub ModelDB.Msg
subscriptions _ =
    Ports.loadJson ModelDB.LoadJson
