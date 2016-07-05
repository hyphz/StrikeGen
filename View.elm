
module View exposing (view)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (map, head, length, filter, sortBy)
import Html.App as Html
import Json.Decode exposing (Decoder, decodeString, (:=), object4, string, list, int, at)
import Result exposing (withDefault)
import ModelDB exposing (..)
import Dict exposing (..)
import Maybe exposing (..)
import CharModel exposing (getSkills, getForms)
import TacticalModel exposing (getPowers)
import FormsModel exposing (..)
import Color
import String
import Svg exposing (svg)
import Material.Icons.Action exposing (delete)
import Material.Icons.Content exposing (add_box)
import Svg.Attributes
import Markdown


sourceName : Int -> String
sourceName s = case s of
    0 -> "Background"
    1 -> "Origin"
    2 -> "Starting"
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

dropdownFieldOption : Model -> String -> String -> Html Msg
dropdownFieldOption model key opt =
  let isSelected = case (Dict.get key model.character) of
    Just x -> opt == x
    Nothing -> False
  in option [selected isSelected] [text opt]

useIcon : (Color.Color -> Int -> Svg.Svg a) -> Int -> Html a
useIcon icon size = Svg.svg [Svg.Attributes.height (toString size),
    Svg.Attributes.width (toString size)] [icon Color.black size]

formFieldDisplay : Model -> Field -> Html Msg
formFieldDisplay model field =
  let
    colSpanForDel = case (fieldDel field) of
      True -> [(Html.Attributes.colspan 1)]
      False -> [(Html.Attributes.colspan 2)]
    delCol = case (fieldDel field) of
      True -> [td [Html.Attributes.width 20,
                  Html.Events.onClick (FieldDeleteClicked (fieldKey field))] [useIcon delete 20]]
      False -> []
  in case field of
  FreeformField ff -> tr [] ([td [] [(text ff.name)], td colSpanForDel
    [input [(Html.Events.on "change" (targetAndWrap (FormFieldUpdated ff.key))),
            (Html.Attributes.value (Maybe.withDefault "" (get ff.key model.character)))] []]]
    ++ delCol)

  DropdownField df -> tr [] ([td [] [(text df.name)], td colSpanForDel
    [select [(Html.Events.on "change" (targetAndWrap (FormFieldUpdated df.key)))]
             (List.map ((dropdownFieldOption model) df.key) df.choices)]]
           ++ delCol)
  NumberField nf -> tr [] ([td [] [(text nf.name)], td colSpanForDel
    [input [(Html.Events.on "change" (targetAndWrap (FormFieldUpdated nf.key))),
            (Html.Attributes.value (Maybe.withDefault "" (get nf.key model.character))),
            (Html.Attributes.type' "number"),
            (Html.Attributes.min <| toString nf.min),
            (Html.Attributes.max <| toString nf.max)] []]]
            ++ delCol)

formDisplay : Model -> Form -> Html Msg
formDisplay model form =
  let
    colSpanForAdd = case (form.addable) of
      False -> [Html.Attributes.colspan 3]
      True -> [Html.Attributes.colspan 2]
    addCol = case (form.addable) of
      False -> []
      True -> [th [Html.Attributes.width 20,
                   Html.Events.onClick (FormAddClicked form.name)] [useIcon add_box 20]]
  in
    table [(Html.Attributes.class "form")]
        [thead [] [tr [] ([th colSpanForAdd [text form.name]] ++ addCol)],
            tbody [] (List.map (formFieldDisplay model) (form.fields))]


mdOptions : Markdown.Options
mdOptions = { githubFlavored = Nothing, defaultHighlighting = Nothing,
              sanitize = True, smartypants = False }

powerCard : Power -> Html Msg
powerCard power =
  let
    cardCssClass = case power.styl of
      Yellow -> "yellowpower"
      Red -> "redpower"
      Blue -> "bluepower"
      Green -> "greenpower"
      White -> "powerwhite" -- Not going there
      Purple -> "purplepower"
    typeIcon = case power.slot of
      Attack -> "attack.svg"
      RoleSlot -> "role.svg"
      Misc -> "circle.svg"
      Special -> "circle.svg"
      Reaction -> "reaction.svg"
    freqText = case power.freq of
      AtWill -> "At-Will"
      Encounter -> "Encounter"
      None -> ""
    attackTypeIcon = case power.slot of
      Attack -> case power.range of
        0 -> if (power.area == 0) then
               [img [src "icons/melee.svg", height 16, width 16] []]
             else []
        x -> if (x > 0) then
              [img [src "icons/range.svg", height 16, width 16] [],
                (text (toString power.range))]
             else
              [img [src "icons/melee.svg", height 16, width 16] [],
                text("/"),
               img [src "icons/range.svg", height 16, width 16] [],
                (text (toString (-power.range)))]
      _ -> []
    areaIcon = case power.area of
      0 -> []
      x -> [img [src "icons/area.svg", height 16, width 16] [],
             (text (toString power.area))]
    damageIcon = case power.damage of
      0 -> []
      (-1) -> [img [src "icons/damage.svg", height 16, width 16] [],
             (text "S")]
      x -> [img [src "icons/damage.svg", height 16, width 16] [],
             (text (toString power.damage))]
    iconblock = attackTypeIcon ++ areaIcon ++ damageIcon
  in
  div [Html.Attributes.class ("powerbox " ++ cardCssClass)] [
    div [Html.Attributes.class "powerhead"] [
      div [Html.Attributes.class "powerti"] [
        img [src ("icons/"++typeIcon), height 16, width 16] []
      ],
      div [Html.Attributes.class "powername"] [text power.name],
      div [class "powericons"] iconblock,
      div [class "powertype"] [text freqText]
    ],
    div [class "powertext"] [Markdown.toHtmlWith mdOptions [] power.text]
  ]

powerOrder : Power -> Int
powerOrder power =
  case power.styl of
    White -> 0
    Green -> 1
    Blue -> 2
    Purple -> 3
    Red -> 4
    Yellow -> 5

powerCards : Model -> Html Msg
powerCards model =
    div [Html.Attributes.class "powercards"] (List.map powerCard (List.sortBy powerOrder (TacticalModel.getPowers model)))


fileops : Html Msg
fileops = select [(Html.Events.on "change" (targetAndWrap FileCommand)),Html.Attributes.id "fileops"]
           [(option [selected True, value "nil"] [text "File..."]),
            (option [selected False, value "download"] [text "Download to local file"]),
            (option [selected False, value "upload"] [text "Upload from local file"]),
            (option [selected False, value "seturl"] [text "Save to URL (bookmark to store)"]),
            (option [selected False, value "roll20"] [text "Export Roll20 macros (text file)"]),
            (option [selected False, value "reset"] [text "Clear character (no confirm - save first!)"])]

formsDisplay : Model -> Html Msg
formsDisplay model = div [] ([fileops] ++ (List.map (formDisplay model) (getForms model)))

view : Model -> Html Msg
view model = div [] [div [Html.Attributes.class "forms"] [formsDisplay model],
                     div [Html.Attributes.class "sheet"] [skillTable model, powerCards model]]
