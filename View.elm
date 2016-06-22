
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
import CharModel exposing (getSkills, getForms, getPowers)
import FormsModel exposing (..)
import Color
import Svg exposing (svg)
import Material.Icons.Action exposing (delete)
import Material.Icons.Content exposing (add_box)
import Svg.Attributes


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
      True -> [td [Html.Attributes.width 20] [useIcon delete 20]]
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

powerCard power =
  let
    cardCssClass = case power.freq of
      AtWill -> case power.slot of
        Attack -> "atwillattack"
        Role -> "roleatwill"
        Misc -> "roleatwill"
        Reaction -> "special"
        Special -> "special"
      Encounter -> case power.slot of
        Attack -> "encattack"
        Role -> "roleenc"
        Misc -> "encmisc"
        Special -> "special"
        Reaction -> "encmisc"
      None -> "special"
    typeIcon = case power.slot of
      Attack -> "attack.svg"
      Role -> "role.svg"
      Misc -> "circle.svg"
      Special -> "circle.svg"
      Reaction -> "reaction.svg"
    freqText = case power.freq of
      AtWill -> "At-Will"
      Encounter -> "Encounter"
      None -> ""
    attackTypeIcon = case power.slot of
      Attack -> case power.range of
        0 -> [img [src "icons/melee.svg", height 16, width 16] []]
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
    div [class "powertext"] [text power.text]
  ]

powerCards model =
  let
    specials = List.filter (\x -> x.slot == Special) (getPowers model)
    atwills = List.filter (\x -> x.freq == AtWill) (getPowers model)
    encounters = List.filter (\x -> x.freq == Encounter) (getPowers model)
    weirdoes = List.filter (\x -> (x.freq == None) && (x.slot /= Special)) (getPowers model)
  in
    div [Html.Attributes.class "powercards"] (List.map powerCard (specials ++ atwills ++ encounters ++ weirdoes))

fileops : Html Msg
fileops = button [onClick DoSave] [text "Download"]

formsDisplay : Model -> Html Msg
formsDisplay model = div [] ([fileops] ++ (List.map (formDisplay model) (getForms model)))

view : Model -> Html Msg
view model = div [] [div [Html.Attributes.class "forms"] [formsDisplay model],
                     div [Html.Attributes.class "sheet"] [skillTable model, powerCards model]]
