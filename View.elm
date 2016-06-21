
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
import Types exposing (..)
import Dict exposing (..)
import Maybe exposing (..)




-- Some utilities for quickly making strings into HTML options for use in selects.
stringToOption : String -> Html msg
stringToOption s = option [] [text s]

stringsToOptions : List String -> List (Html msg)
stringsToOptions = List.map stringToOption

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
skillTable m = table [] [thead [] [skillTableHeader], (tbody [] (List.map skillToHtmlTableRow (sortBy .name m.character.skills)))]


-- For some reason Elm modified the HTML library so that instead of passing Html.Events.on a function to apply
-- to the decoded value, you have to roll it into the decoder. So here's a function that rolls another function
-- into a targetvalue decoder.
targetAndWrap : (String -> Msg) -> Decoder Msg
targetAndWrap f = Json.Decode.map f targetValue

maybeToResult : String -> (Maybe a) -> (Result String a)
maybeToResult err may = case may of
    Just x -> Ok x
    Nothing -> Err err

-- Gets the one member of a list that satisfies a condition, or throws an error if there's none or more than
-- one.
theOne : (a -> Bool) -> List a -> (Result String a)
theOne f l =
    let filteredList = (List.filter f l) in
    case (length filteredList) of
        2 -> Err "Multiple entries matched theOne"
        _ -> maybeToResult "No entry matched theOne" (head filteredList)


-- Used within genericListPicker to generate the individual options.
genericListOption : (a -> String) -> (Model -> a) -> Model -> a -> Html Msg
genericListOption nameExtractor modelComponent model x =
    option [selected ((modelComponent model) == x)] [text (nameExtractor x)]

-- Generates a list picker.
-- nameExtractor: function to apply to a model member to get the name to show in the list.
-- modelComponent: function to apply to a model to get the member to refer to.
-- messageGenerator: function to apply to the chosen option to get the message to send.
-- theList: list of components to pick from.
genericListPicker : (a -> String) -> (Model -> a) -> (String -> Msg) -> List a -> Model -> Html Msg
genericListPicker nameExtractor modelComponent messageGenerator theList model =
    select [(Html.Events.on "change" (targetAndWrap messageGenerator))]
      (List.map (genericListOption nameExtractor modelComponent model) (sortBy nameExtractor theList))

backgroundPicker : Model -> Html Msg
backgroundPicker model = genericListPicker .name (.character >> .background) BackgroundChanged model.database.backgrounds model

originPicker : Model -> Html Msg
originPicker model = genericListPicker .name (.character >> .origin) OriginChanged model.database.origins model

formFieldDisplay : Model -> Field -> Html Msg
formFieldDisplay model field = case field of
  FreeformField ff -> tr [] [td [] [(text ff.name)], td []
    [input [(Html.Events.on "change" (targetAndWrap (FreeformFieldUpdated ff.key))),
            (Html.Attributes.value (Maybe.withDefault "" (get ff.key model.character.formresponses)))] []]]
  DropdownField df -> tr [] [td [] [(text df.name)]]

formDisplay : Model -> Form -> Html Msg
formDisplay model form = table [] [thead [] [th [] [text form.name]],
                                   tbody [] (List.map (formFieldDisplay model) (values form.fields))]

formDisplayByKey : Model -> String -> Html Msg
formDisplayByKey model key = case (get key model.database.forms) of
  Just form -> formDisplay model form
  Nothing -> text ("Missing form " ++ key ++ "!")


formsDisplay : Model -> Html Msg
formsDisplay model = div [] (List.map (formDisplayByKey model) model.character.formkeys)


view : Model -> Html Msg
view model = div [] [backgroundPicker model, originPicker model, skillTable model, formsDisplay model]
