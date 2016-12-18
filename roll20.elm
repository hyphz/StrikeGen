module Roll20 exposing (..)

import ModelDB exposing (..)
import TacticalModel exposing (..)
import String


{-| Add spaces at carriage returns in a string, used to format markdown
strings from the database for plain text viewing.
-}
tidyEdges : String -> String
tidyEdges s =
    s |> String.split "\n" |> String.join " "


{-| Return all parts of a string EXCEPT the selected area.
-}
unSlice : Int -> Int -> String -> String
unSlice start end str =
    (String.slice 0 start str) ++ (String.slice end -1 str)


{-| Select a headed paragraph from the markdown text in the text database, and
extract it, removing the paragraph header. Returns the extracted paragraph and
the text with the extracted paragraph eliminated.
-}
textExtract : String -> String -> ( Maybe String, String )
textExtract section text =
    let
        start =
            List.head (String.indexes ("**" ++ section ++ ":**") text)

        possEnds =
            (String.indexes "\n\n" text) ++ (String.indexes "\x0D\n\x0D\n" text) ++ [ String.length text ]

        realEnd =
            case start of
                Nothing ->
                    Nothing

                Just st ->
                    List.head (List.filter (\x -> x > st) possEnds)

        offset =
            (String.length section) + 5
    in
        case start of
            Nothing ->
                ( Nothing, text )

            Just s ->
                case realEnd of
                    Nothing ->
                        ( Nothing, text )

                    Just en ->
                        ( Just (tidyEdges (String.slice (s + offset) en text)), unSlice s en text )


{-| Perform textExtract and create a roll20 macro component if the named section was
found, or a blank string if it wasn't.
-}
extractToMacroPart : String -> String -> ( String, String )
extractToMacroPart name string =
    case textExtract name string of
        ( Nothing, x ) ->
            ( "", x )

        ( Just text, x ) ->
            ( "{{" ++ name ++ "=" ++ text ++ "}}", x )


{-| Export a power as a Roll20 macro.
-}
powerMacro : Power -> String
powerMacro power =
    let
        rangeDesc =
            case power.slot of
                Attack ->
                    if power.range == 0 then
                        "{{Range=Melee}}"
                    else if power.range > 0 then
                        ("{{Range=Ranged " ++ toString power.range ++ "}}")
                    else
                        ("{{Range=Melee or Ranged " ++ toString power.range ++ "}}")

                _ ->
                    if power.range > 0 then
                        ("{{Range=Ranged " ++ toString power.range ++ "}}")
                    else if power.range < 0 then
                        ("{{Range=Adjacent or ranged " ++ toString (-power.range) ++ "}}")
                    else
                        ""

        typeDesc =
            case power.slot of
                Attack ->
                    "{{Type=Attack}}"

                RoleSlot ->
                    "{{Type=Role}}"

                Reaction ->
                    "{{Type=Reaction}}"

                _ ->
                    ""

        attackRoll =
            case power.slot of
                Attack ->
                    "{{Attack=[[1d6]]}}"

                _ ->
                    ""

        areaDesc =
            case power.area of
                0 ->
                    ""

                _ ->
                    "{{Area=" ++ (toString power.area) ++ "}}"

        damageDesc =
            case power.damage of
                0 ->
                    ""

                (-1) ->
                    "{{Damage=Support}}"

                _ ->
                    "{{Damage=" ++ (toString power.damage) ++ "}}"

        ( triggerDesc, triggerRest ) =
            extractToMacroPart "Trigger" power.text

        ( effectDesc, effectRest ) =
            extractToMacroPart "Effect" triggerRest

        ( specialDesc, specialRest ) =
            extractToMacroPart "Special" effectRest

        ( martialDesc, martialRest ) =
            extractToMacroPart "Melee Effect" specialRest

        ( marconDesc, marconRest ) =
            extractToMacroPart "Continuous" martialRest

        restText =
            if (effectRest /= "") then
                "{{Text=" ++ tidyEdges marconRest ++ "}}"
            else
                ""
    in
        "&{template:default}{{name="
            ++ power.name
            ++ "}}"
            ++ typeDesc
            ++ rangeDesc
            ++ areaDesc
            ++ attackRoll
            ++ damageDesc
            ++ triggerDesc
            ++ effectDesc
            ++ specialDesc
            ++ martialDesc
            ++ marconDesc
            ++ restText
            ++ "\x0D\n\x0D\n"


{-| Create the power macro data file for export.
-}
powerMacros : Model -> String
powerMacros model =
    String.concat (List.map powerMacro (TacticalModel.getAllPowers model))
