
module View exposing (view)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (map, head, length, filter, sortBy)
import Html.App as Html
import Json.Decode exposing (Decoder, decodeString, (:=), object4, string, list, int, at)
import Http exposing (getString, Error)
import Task exposing (perform)
import Result exposing (withDefault)
import ModelDB exposing (..)
import Dict exposing (..)
import Maybe exposing (..)
import CharModel exposing (getSkills, getForms)
import FormsModel exposing (..)

sourceName : Int -> String
sourceName s = case s of
    0 -> "Background"
    1 -> "Origin"
    _ -> "Broken"

-- Generates the HTML skill table row for a skill.
skillToHtmlTableRow : Skill -> Html Msg
skillToHtmlTableRow s = tr [] [(td [] [text s.name]),
                               (td [] [text (sourceName s.source)])]

quickTh : String -> Html Msg
quickTh s = th [] [text s]

skillTableHeader : Html Msg
skillTableHeader = tr [] (List.map quickTh ["Name", "Source"])

skillTable : Model -> Html Msg
skillTable m =
  table [] [thead [] [skillTableHeader], (tbody [] (List.map skillToHtmlTableRow (sortBy .name (getSkills m))))]


-- For some reason Elm modified the HTML library so that instead of passing Html.Events.on a function to apply
-- to the decoded value, you have to roll it into the decoder. So here's a function that rolls another function
-- into a targetvalue decoder.
targetAndWrap : (String -> Msg) -> Decoder Msg
targetAndWrap f = Json.Decode.map f targetValue

dropdownFieldOption model key opt =
  let isSelected = case (Dict.get key model.character) of
    Just x -> opt == x
    Nothing -> False
  in option [selected isSelected] [text opt]

formFieldDisplay : Model -> Field -> Html Msg
formFieldDisplay model field = case field of
  FreeformField ff -> tr [] [td [] [(text ff.name)], td []
    [input [(Html.Events.on "change" (targetAndWrap (FormFieldUpdated ff.key))),
            (Html.Attributes.value (Maybe.withDefault "" (get ff.key model.character)))] []]]
  DropdownField df -> tr [] [td [] [(text df.name)], td []
    [select [(Html.Events.on "change" (targetAndWrap (FormFieldUpdated df.key)))]
             (List.map ((dropdownFieldOption model) df.key) df.choices)]]
  NumberField nf -> tr [] [td [] [(text nf.name)], td []
    [input [(Html.Events.on "change" (targetAndWrap (FormFieldUpdated nf.key))),
            (Html.Attributes.value (Maybe.withDefault "" (get nf.key model.character))),
            (Html.Attributes.type' "number"),
            (Html.Attributes.min <| toString nf.min),
            (Html.Attributes.max <| toString nf.max)] []]]

formDisplay : Model -> Form -> Html Msg
formDisplay model form = table [(Html.Attributes.class "form")] [thead [] [th [Html.Attributes.colspan 2] [text form.name]],
                                   tbody [] (List.map (formFieldDisplay model) (form.fields))]

formsDisplay : Model -> Html Msg
formsDisplay model = div [] (List.map (formDisplay model) (getForms model))

view : Model -> Html Msg
view model = div [] [div [Html.Attributes.class "forms"] [formsDisplay model], div [Html.Attributes.class "sheet"] [skillTable model]]
