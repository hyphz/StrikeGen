module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import List exposing (map, head, length, filter, sortBy)
import ModelDB exposing (..)
import Dict exposing (..)
import Maybe exposing (..)
import CharModel exposing (getSkills, getForms, getWealth, getTricks, getComplications)
import TacticalModel exposing (getPowers, isMultiRoleShaper, getHP, getSpeed)
import FormsModel exposing (..)
import Color
import Svg exposing (svg)
import Material.Icons.Action exposing (delete)
import Material.Icons.Content exposing (add_box)
import Svg.Attributes
import Markdown


{-| Returns a row with a header and text. Used for the basics table.
-}
headerTableRow : String -> String -> Html Msg
headerTableRow label value =
    tr [] [ (th [] [ text label ]), (td [] [ text value ]) ]


{-| Placeholder for characters with no name. Yes, cheesy.
-}
noName : Model -> String
noName m =
    case (getResponse m "basics-origin") of
        Nothing ->
            "Amorphous Blob Of Infinite Possibility"

        Just "<Not Selected>" ->
            "Amorphous Blob of Infinite Possibility"

        Just x ->
            "The " ++ x ++ " With No Name"


{-| Text for the Role entry. As usual, those pesky MRSs get us.
-}
roleText : Model -> String
roleText m =
    case (isMultiRoleShaper m) of
        True ->
            "Multi-Role Shapeshifter"

        False ->
            Maybe.withDefault "" (getResponse m "basics-role")


{-| Gets the name given to each wealth level.
-}
wealthDesc : Int -> String
wealthDesc w =
    case w of
        0 ->
            " (Penniless)"

        1 ->
            " (Poor)"

        2 ->
            " (Rich)"

        3 ->
            " (Super-Rich)"

        _ ->
            " (BUG)"


{-| The top header of the character sheet with basic RP material.
-}
sheetHeader : Model -> Html Msg
sheetHeader m =
    table [ class "allheader" ]
        [ tr []
            [ td []
                [ table [ class "sheetheader" ]
                    [ headerTableRow "Name:" (Maybe.withDefault (noName m) (getResponse m "basics-name"))
                    , headerTableRow "Background:" (Maybe.withDefault "" (getResponse m "basics-bg"))
                    , headerTableRow "Origin:" (Maybe.withDefault "" (getResponse m "basics-origin"))
                    , headerTableRow "Level:" (toString <| getLevel m)
                    , headerTableRow "Wealth:" ((toString <| getWealth m) ++ (wealthDesc (getWealth m)))
                    ]
                ]
            , td []
                [ table [ class "tacticalheader" ]
                    [ headerTableRow "Class:" (Maybe.withDefault "" (getResponse m "basics-class"))
                    , headerTableRow "Role:" (roleText m)
                    , headerTableRow "Speed:" (toString <| getSpeed m)
                    , headerTableRow "HP:" (toString <| getHP m)
                    ]
                ]
            ]
        ]


{-| CSS class for sourced values in tables.
-}
classForSrc : Int -> String
classForSrc s =
    case s of
        0 ->
            "backgroundSkill"

        1 ->
            "originSkill"

        2 ->
            "userSkill"

        _ ->
            "Broken"


{-| The row of ticks used next to skills and tricks in organic advancement.
-}
ticksRow : List (Html Msg)
ticksRow =
    List.repeat 10 (img [ src "icons/square.svg", height 16, width 16 ] [])



{-| Generates the HTML skill table row for a skill.
-}
sourcedToHtmlTableRow : Model -> Bool -> Sourced -> Html Msg
sourcedToHtmlTableRow m o s =
    tr []
        ([ (td [ class (classForSrc s.source) ] [ text s.name ]) ]
            ++ if (o && organicStyleDisplay m) then
                [ td [ class (classForSrc s.source) ] ticksRow ]
               else
                []
        )


{-| Header for a table of sourced data.
-}
sourcedTableHeader : Model -> String -> Bool -> Html Msg
sourcedTableHeader m h o =
    case (o && organicStyleDisplay m) of
        False ->
            tr [] [ th [] [ text h ] ]

        True ->
            tr [] [ th [ colspan 2 ] [ text h ] ]


{-| Generic table of sourced data.
-}
sourcedTable : Model -> List Sourced -> String -> Bool -> Html Msg
sourcedTable m l h o =
    table [] [ thead [] [ sourcedTableHeader m h o ], (tbody [] (List.map (sourcedToHtmlTableRow m o) (sortBy .name l))) ]


{-| The three main tables of sourced data.
-}
skillTable : Model -> Html Msg
skillTable m =
    div [ class "skilltable" ] [ sourcedTable m (getSkills m) "Skills" True ]


trickTable : Model -> Html Msg
trickTable m =
    div [ class "tricktable" ] [ sourcedTable m (getTricks m) "Tricks" True ]


{-| The complication table is a bit different because it doesn't have tick rows, even
if Organic advancement is being used.
-}
compTable : Model -> Html Msg
compTable m =
    div [ class "comptable" ]
        ([ sourcedTable m (getComplications m) "Complications" False ]
            ++ if (organicStyleDisplay m) then
                [ combatApLine ]
               else
                []
        )


{-| The "AP spent in combat" tick line for organic advancement.
-}
combatApLine : Html Msg
combatApLine =
    p [] ([ text "AP spent in combat:" ] ++ ticksRow)



-- For some reason Elm modified the HTML library so that instead of passing Html.Events.on a function to apply
-- to the decoded value, you have to roll it into the decoder. So here's a function that rolls another function
-- into a targetvalue decoder.


targetAndWrap : (String -> Msg) -> Json.Decode.Decoder Msg
targetAndWrap f =
    Json.Decode.map f targetValue


dropdownFieldOption : Model -> String -> String -> Html Msg
dropdownFieldOption model key opt =
    let
        isSelected =
            case (Dict.get key model.character) of
                Just x ->
                    opt == x

                Nothing ->
                    False
    in
        option [ selected isSelected ] [ text opt ]


{-| Use one of the icons from the Google icon library included as elm. NOT for regular
SVG icons!
-}
useIcon : (Color.Color -> Int -> Svg.Svg a) -> Int -> Html a
useIcon icon size =
    Svg.svg
        [ Svg.Attributes.height (toString size)
        , Svg.Attributes.width (toString size)
        ]
        [ icon Color.black size ]


formFieldDisplay : Model -> Field -> Html Msg
formFieldDisplay model field =
    let
        colSpanForDel =
            case (fieldDel field) of
                True ->
                    [ (Html.Attributes.colspan 1) ]

                False ->
                    [ (Html.Attributes.colspan 2) ]

        delCol =
            case (fieldDel field) of
                True ->
                    [ td
                        [ Html.Attributes.width 20
                        , Html.Events.onClick (FieldDeleteClicked (fieldKey field))
                        ]
                        [ useIcon delete 20 ]
                    ]

                False ->
                    []

        debugFieldKeys =
            case (getResponse model "basics-name") of
                Just "debugfieldkeys" ->
                    .key

                _ ->
                    .name
    in
        case field of
            FreeformField ff ->
                tr []
                    ([ td [] [ (text (debugFieldKeys ff)) ]
                     , td colSpanForDel
                        [ input
                            [ (Html.Events.on "change" (targetAndWrap (FormFieldUpdated ff.key)))
                            , (Html.Attributes.value (Maybe.withDefault "" (get ff.key model.character)))
                            ]
                            []
                        ]
                     ]
                        ++ delCol
                    )

            DropdownField df ->
                tr []
                    ([ td [] [ (text (debugFieldKeys df)) ]
                     , td colSpanForDel
                        [ select [ (Html.Events.on "change" (targetAndWrap (FormFieldUpdated df.key))) ]
                            (List.map ((dropdownFieldOption model) df.key) df.choices)
                        ]
                     ]
                        ++ delCol
                    )

            NumberField nf ->
                tr []
                    ([ td [] [ (text (debugFieldKeys nf)) ]
                     , td colSpanForDel
                        [ input
                            [ (Html.Events.on "change" (targetAndWrap (FormFieldUpdated nf.key)))
                            , (Html.Attributes.value (Maybe.withDefault "" (get nf.key model.character)))
                            , (Html.Attributes.type_ "number")
                            , (Html.Attributes.min <| toString nf.min)
                            , (Html.Attributes.max <| toString nf.max)
                            ]
                            []
                        ]
                     ]
                        ++ delCol
                    )


formDisplay : Model -> Form -> Html Msg
formDisplay model form =
    let
        colSpanForAdd =
            case (form.addable) of
                False ->
                    [ Html.Attributes.colspan 3 ]

                True ->
                    [ Html.Attributes.colspan 2 ]

        addCol =
            case (form.addable) of
                False ->
                    []

                True ->
                    [ th
                        [ Html.Attributes.width 20
                        , Html.Events.onClick (FormAddClicked form.name)
                        ]
                        [ useIcon add_box 20 ]
                    ]
    in
        table [ (Html.Attributes.class "form") ]
            [ thead [] [ tr [] ([ th colSpanForAdd [ text form.name ] ] ++ addCol) ]
            , tbody [] (List.map (formFieldDisplay model) (form.fields))
            ]


mdOptions : Markdown.Options
mdOptions =
    { githubFlavored = Nothing
    , defaultHighlighting = Nothing
    , sanitize = True
    , smartypants = False
    }


powerCard : Power -> Html Msg
powerCard power =
    let
        cardCssClass =
            case power.styl of
                Yellow ->
                    "yellowpower"

                Red ->
                    "redpower"

                Blue ->
                    "bluepower"

                Green ->
                    "greenpower"

                White ->
                    "powerwhite"

                -- Not going there
                Purple ->
                    "purplepower"

        typeIcon =
            case power.slot of
                Attack ->
                    "attack.svg"

                RoleSlot ->
                    "role.svg"

                Misc ->
                    "circle.svg"

                Special ->
                    "circle.svg"

                Reaction ->
                    "reaction.svg"

                Move ->
                    "move.svg"

        freqText =
            case power.freq of
                AtWill ->
                    "At-Will"

                Encounter ->
                    "Encounter"

                None ->
                    ""

        attackTypeIcon =
            if (power.slot == Attack || power.slot == RoleSlot) then
                case power.range of
                    0 ->
                        if ((power.area == 0) && (power.slot == Attack) && (power.damage /= 0)) then
                            [ img [ src "icons/melee.svg", height 16, width 16 ] [] ]
                        else
                            []

                    x ->
                        if (x > 0) then
                            [ img [ src "icons/range.svg", height 16, width 16 ] []
                            , (text (toString power.range))
                            ]
                        else
                            [ img [ src "icons/melee.svg", height 16, width 16 ] []
                            , text ("/")
                            , img [ src "icons/range.svg", height 16, width 16 ] []
                            , (text (toString (-power.range)))
                            ]
            else
                []

        areaIcon =
            case power.area of
                0 ->
                    []

                x ->
                    [ img [ src "icons/area.svg", height 16, width 16 ] []
                    , (text (toString power.area))
                    ]

        damageIcon =
            case power.damage of
                0 ->
                    []

                (-1) ->
                    [ img [ src "icons/damage.svg", height 16, width 16 ] []
                    , (text "S")
                    ]

                x ->
                    [ img [ src "icons/damage.svg", height 16, width 16 ] []
                    , (text (toString power.damage))
                    ]

        iconblock =
            attackTypeIcon ++ areaIcon ++ damageIcon
    in
        div [ Html.Attributes.class ("powerbox " ++ cardCssClass) ]
            [ div [ Html.Attributes.class "powerhead" ]
                [ div [ Html.Attributes.class "powerti" ]
                    [ img [ src ("icons/" ++ typeIcon), height 16, width 16 ] []
                    ]
                , div [ Html.Attributes.class "powername" ] [ text power.name ]
                , div [ class "powericons" ] iconblock
                , div [ class "powertype" ] [ text freqText ]
                ]
            , div [ class "powertext" ] [ Markdown.toHtmlWith mdOptions [] power.text ]
            ]


powerOrder : Power -> Int
powerOrder power =
    case power.styl of
        White ->
            0

        Green ->
            1

        Blue ->
            2

        Purple ->
            3

        Red ->
            4

        Yellow ->
            5


organicStyleDisplay : Model -> Bool
organicStyleDisplay m =
    case (getResponse m "basics-ticks") of
        Just "Yes" ->
            True

        _ ->
            False


powerCards : Model -> Html Msg
powerCards model =
    div [ Html.Attributes.class "powercards" ] (List.map powerCard (List.sortBy powerOrder (TacticalModel.getPowers model)))


powerBlock : PowerBlock -> Html Msg
powerBlock block =
    div [ Html.Attributes.class "powerblock" ]
        [ div [ Html.Attributes.class "powerblocktitle" ] [ text block.name ]
        , div [ Html.Attributes.class "powercards" ] (List.map powerCard block.powers)
        ]


powerBlocks : Model -> Html Msg
powerBlocks m =
    div [ Html.Attributes.class "powerblocks" ] (List.map powerBlock (TacticalModel.getPowerBlocks m))


kitTableRow : Model -> String -> Int -> List (Html Msg)
kitTableRow m key lev =
    case (getResponse m key) of
        Nothing ->
            []

        Just adv ->
            [ tr [] [ td [] [ text ((toString lev) ++ ":") ], td [] [ text adv ] ] ]


kitTableBody : Model -> List (Html Msg)
kitTableBody m =
    (kitTableRow m "kit-start" 1)
        ++ (kitTableRow m "kit-1" 1)
        ++ (if ((getLevel m) >= 3) then
                (kitTableRow m "kit-2" 3)
            else
                []
           )
        ++ (if ((getLevel m) >= 5) then
                (kitTableRow m "kit-3" 5)
            else
                []
           )
        ++ (if ((getLevel m) >= 7) then
                (kitTableRow m "kit-4" 7)
            else
                []
           )
        ++ (if ((getLevel m) >= 9) then
                (kitTableRow m "kit-5" 9)
            else
                []
           )


kitTable : Model -> Html Msg
kitTable m =
    div [ class "kittable" ] [ table [] [ thead [] [ tr [] [ th [ colspan 2 ] [ text "Kits and Advances" ] ] ], tbody [] (kitTableBody m) ] ]


repTableRow : Model -> Int -> List (Html Msg)
repTableRow m i =
    case (getResponse m ("rep-" ++ (toString i))) of
        Nothing ->
            []

        Just repname ->
            case (getResponse m ("rept-" ++ (toString i))) of
                Nothing ->
                    []

                Just reptype ->
                    if (reptype == "") then
                        []
                    else
                        [ tr [] [ td [] [ text repname ], td [] [ text (reptype) ] ] ]


repTableBody : Model -> List (Html Msg)
repTableBody m =
    repTableRow m 1
        ++ (if ((getLevel m) >= 4) then
                (repTableRow m 2)
            else
                []
           )
        ++ (if ((getLevel m) >= 6) then
                (repTableRow m 3)
            else
                []
           )
        ++ (if ((getLevel m) >= 8) then
                (repTableRow m 4)
            else
                []
           )
        ++ (if ((getLevel m) >= 10) then
                (repTableRow m 5)
            else
                []
           )


repTable : Model -> List (Html Msg)
repTable m =
    if ((getLevel m) < 2) then
        []
    else
        [ div [ class "reptable" ] [ table [] [ thead [] [ tr [] [ th [ colspan 2 ] [ text "Reputation" ] ] ], tbody [] (repTableBody m) ] ] ]


fallbackDisplay : Model -> List (Html Msg)
fallbackDisplay m =
    if ((getLevel m) < 2) then
        []
    else
        case (getResponse m "adv-fallback") of
            Just fb ->
                [ p [] [ b [] [ text "Fallback: " ], text fb ] ]

            Nothing ->
                []


fileops : Html Msg
fileops =
    select [ (Html.Events.on "change" (targetAndWrap FileCommand)), Html.Attributes.id "fileops" ]
        [ (option [ selected True, value "nil" ] [ text "File..." ])
        , (option [ selected False, value "download" ] [ text "Download to local file" ])
        , (option [ selected False, value "upload" ] [ text "Upload from local file" ])
        , (option [ selected False, value "seturl" ] [ text "Save to URL (bookmark to store)" ])
        , (option [ selected False, value "print" ] [ text "Print (to printer, PDF, etc..)" ])
        , (option [ selected False, value "roll20" ] [ text "Export Roll20 macros (text file)" ])
        , (option [ selected False, value "reset" ] [ text "Clear character (no confirm - save first!)" ])
        ]


formsDisplay : Model -> Html Msg
formsDisplay model =
    div [] ([ fileops ] ++ (List.map (formDisplay model) (getForms model)))


view : Model -> Html Msg
view model =
    div []
        [ div [ Html.Attributes.class "forms" ] [ formsDisplay model ]
        , div [ Html.Attributes.class "sheet" ] ([ sheetHeader model ] ++ fallbackDisplay model ++ [ div [ class "sstables" ] [ skillTable model, compTable model ], div [ class "tktables" ] [ trickTable model, kitTable model ] ] ++ repTable model ++ [ powerCards model, powerBlocks model ])
        ]
