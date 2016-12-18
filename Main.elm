module Main exposing (main)

import Html
import View
import ModelDB
import CharModel
import Ports

main : Program Never ModelDB.Model ModelDB.Msg
main =
    Html.program { init = CharModel.init, view = View.view, update = CharModel.update, subscriptions = subscriptions }


subscriptions : ModelDB.Model -> Sub ModelDB.Msg
subscriptions _ =
    Ports.loadJson ModelDB.LoadJson
