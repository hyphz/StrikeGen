module Kits exposing (..)

import ModelDB exposing (..)
import Dict
import String
import FormsModel exposing (..)


{-| Gets the list of kit options chosen up to the l'th slot.
-}
kitChoices : Model -> Int -> List String
kitChoices m l =
    List.foldr (++) [] (List.map (\x -> mayList <| getResponse m ("kit-" ++ (toString x))) (List.range 1 l))


{-| Gets the list of advances for a named kit.
-}
advancesForKit : Model -> String -> List KitAdvance
advancesForKit m k =
    case (Dict.get k m.database.kits) of
        Nothing ->
            []

        Just kit ->
            kit.advances


{-| Checks if the character has the named kit advance in any slot up to the l'th, allowing for poaching.
-}
hasKitAdvanceNamed : Model -> String -> Int -> Bool
hasKitAdvanceNamed m n l =
    (List.member n (kitChoices m l)) || (List.member ("Poach: " ++ n) (kitChoices m l))


{-| Filters a list of kit advances to remove those whose prerequisites are not met in slots up to the l'th.
-}
filterForPrereqs : Model -> Int -> List KitAdvance -> List KitAdvance
filterForPrereqs m l list =
    (List.filter (\x -> List.all (\y -> hasKitAdvanceNamed m y l) x.prereqs) list)


{-| Gets the names of all known kits that are not minikits.
-}
allFullKits : Model -> List String
allFullKits m =
    List.map .name (List.filter (\x -> x.mini == False) (Dict.values m.database.kits))


kitAdvanceOptions : Model -> Int -> List String
kitAdvanceOptions m limit =
    let
        firstKit =
            case (getResponse m "kit-start") of
                Nothing ->
                    []

                Just k ->
                    [ k ]

        extensionKits =
            List.map (String.dropLeft 9) (List.filter (String.startsWith "New Kit: ") (kitChoices m limit))

        allExistingKits =
            firstKit ++ extensionKits

        allAdvancesForExistingKits =
            filterForPrereqs m limit (List.concatMap (advancesForKit m) allExistingKits)

        possibleNewKits =
            List.filter (\x -> not (List.member x allExistingKits)) (allFullKits m)

        poached =
            List.any (String.startsWith "Poach: ") (kitChoices m limit)

        possiblePoaches =
            if poached then
                []
            else
                filterForPrereqs m limit (List.concatMap (advancesForKit m) possibleNewKits)

        miniKitAdvances =
            filterForPrereqs m limit (List.concatMap .advances (List.filter (\x -> x.mini) (Dict.values m.database.kits)))
    in
        (List.sort (List.map .name (allAdvancesForExistingKits ++ miniKitAdvances)))
            ++ (List.sort (List.map (\x -> "New Kit: " ++ x) possibleNewKits))
            ++ (List.sort (List.map (\x -> "Poach: " ++ x.name) possiblePoaches))


kitsForm : Model -> Form
kitsForm m =
    Form False
        "Kits"
        ([ DropdownField { name = "Kit:", del = False, key = "kit-start", choices = [ "" ] ++ (allFullKits m) }
         , DropdownField { name = "Advance:", del = False, key = "kit-1", choices = ([ "" ] ++ kitAdvanceOptions m 0) }
         ]
            ++ (if ((getLevel m) >= 3) then
                    [ DropdownField { name = "Advance:", del = False, key = "kit-2", choices = ([ "" ] ++ (kitAdvanceOptions m 1)) } ]
                else
                    []
               )
            ++ (if ((getLevel m) >= 5) then
                    [ DropdownField { name = "Advance:", del = False, key = "kit-3", choices = ([ "" ] ++ (kitAdvanceOptions m 2)) } ]
                else
                    []
               )
            ++ (if ((getLevel m) >= 7) then
                    [ DropdownField { name = "Advance:", del = False, key = "kit-4", choices = ([ "" ] ++ (kitAdvanceOptions m 3)) } ]
                else
                    []
               )
            ++ (if ((getLevel m) >= 9) then
                    [ DropdownField { name = "Advance:", del = False, key = "kit-5", choices = ([ "" ] ++ (kitAdvanceOptions m 4)) } ]
                else
                    []
               )
        )


validateKitProgression : Model -> Model
validateKitProgression m =
    killOutOfRange "kit-1" (kitAdvanceOptions m 0) <|
        killOutOfRange "kit-2" (kitAdvanceOptions m 1) <|
            killOutOfRange "kit-3" (kitAdvanceOptions m 2) <|
                killOutOfRange "kit-4" (kitAdvanceOptions m 3) <|
                    killOutOfRange "kit-5" (kitAdvanceOptions m 4) m
