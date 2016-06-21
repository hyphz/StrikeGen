import Html.App as Html
import View exposing (..)
import CharModel exposing (..)
import ModelDB exposing (..)


main = Html.program { init = init, view = view, update = update, subscriptions = subscriptions }

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
