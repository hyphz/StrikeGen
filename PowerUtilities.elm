module PowerUtilities exposing (..)

import Dict exposing (Dict, get)
import Maybe exposing (Maybe)
import FormsModel exposing (..)
import ModelDB exposing (..)
import String


-- Functions useful in writing power definitions.


{-| Create a name-indexed dictionary of powers from a list of them.
-}
powerDict : Model -> List (Model -> Power) -> Dict String Power
powerDict m l =
    let
        toTuple p =
            ( (p m).name, p m )
    in
        Dict.fromList (List.map toTuple l)


{-| Look up a field in the character spec, then look up the power named in that
field.
-}
powerlookup : Model -> String -> (Model -> Dict String Power) -> List Power
powerlookup m key list =
    case (getResponse m key) of
        Nothing ->
            []

        Just choice ->
            case (get choice (list m)) of
                Nothing ->
                    []

                Just power ->
                    [ power ]


{-| Like powerlookup, but takes the input key name in two parts.
-}
prefixpowerlookup : Model -> String -> String -> (Model -> Dict String Power) -> List Power
prefixpowerlookup m p key list =
    powerlookup m (p ++ key) list


{-| Create a dropdown field listing all the powers from a powerdict generator
function.
-}
powerChoiceField : Model -> String -> String -> (Model -> Dict String Power) -> Field
powerChoiceField m name key list =
    DropdownField { name = name, del = False, key = key, choices = [ "" ] ++ (Dict.keys (list m)) }


{-| Like powerchoicefield but takes the input key name in two parts.
-}
prefixpowerChoiceField : Model -> String -> String -> String -> (Model -> Dict String Power) -> Field
prefixpowerChoiceField m name p key list =
    powerChoiceField m name (p ++ key) list


{-| Shorthand for a power object with overtext based on name.
-}
quickPower : String -> Slot -> Freq -> Int -> Int -> Int -> PowerStyle -> Model -> Power
quickPower name slot freq range area damage col m =
    { name = name
    , text = overtext m (String.filter (\x -> (x /= ' ')) name)
    , slot = slot
    , freq = freq
    , range = range
    , area = area
    , damage = damage
    , styl = col
    }


{-| Shorthand for a power with variable text.
-}
variableTextPower :
    String
    -> Slot
    -> Freq
    -> Int
    -> Int
    -> Int
    -> PowerStyle
    -> (Model -> String)
    -> Model
    -> Power
variableTextPower name slot freq range area damage col textfunc m =
    { name = name
    , text = textfunc m
    , slot = slot
    , freq = freq
    , range = range
    , area = area
    , damage = damage
    , styl = col
    }


{-| Shorthand for a power with text that changes at particular character level thresholds.
-}
levelTextPower :
    String
    -> Slot
    -> Freq
    -> Int
    -> Int
    -> Int
    -> PowerStyle
    -> List Int
    -> Model
    -> Power
levelTextPower name slot freq range area damage col thresholds model =
    let
        levelLookUp m =
            let
                charLevel =
                    getLevel m

                findThreshold t l =
                    case (List.head t) of
                        Nothing ->
                            l

                        Just x ->
                            if (charLevel >= x) then
                                case (List.tail t) of
                                    Nothing ->
                                        l

                                    Just tail ->
                                        findThreshold tail x
                            else
                                l

                nearestThreshold =
                    findThreshold thresholds 1

                textKey =
                    (String.filter (\x -> (x /= ' ')) name) ++ (toString nearestThreshold) ++ "+"
            in
                overtext m (textKey)
    in
        variableTextPower name slot freq range area damage col (levelLookUp) model


{-| Shorthand for a special ability, ie a power with no stats.
-}
quickSpecial : String -> Model -> Power
quickSpecial name m =
    quickPower name Special None 0 0 0 White m


{-| Shorthand for a special ability with text that changes at character level thresholds.
-}
levelTextSpecial : String -> List Int -> Model -> Power
levelTextSpecial name thresholds m =
    levelTextPower name Special None 0 0 0 White thresholds m


{-| Returns the item given if the current character level is less than the specified level,
otherwise returns nothing. Returns a single-item or empty list for ease of concatting.
-}
atLevel : Model -> Int -> a -> List a
atLevel m level ab =
    if ((getLevel m) >= level) then
        [ ab ]
    else
        []


{-| Same as AtLevel, but doesn't wrap the result in a list. Used when the input is already a
list and the output should be a flat list.
-}
atLevelList : Model -> Int -> List a -> List a
atLevelList m level ab =
    if ((getLevel m) >= level) then
        ab
    else
        []


{-| Gets the list of all selected feats.
-}
chosenFeats : Model -> List String
chosenFeats m =
    mayList (getResponse m "feat-1")
        ++ atLevelList m 3 (mayList (getResponse m "feat-2"))
        ++ atLevelList m 5 (mayList (getResponse m "feat-3"))
        ++ atLevelList m 7 (mayList (getResponse m "feat-4"))
        ++ atLevelList m 9 (mayList (getResponse m "feat-5"))


{-| Checks if the character has a feat or not.
-}
hasFeat : Model -> String -> Bool
hasFeat m f =
    List.member f (chosenFeats m)


{-| Checks if the character doesn't have a feat or not.
-}
lacksFeat : Model -> String -> Bool
lacksFeat m f =
    not (hasFeat m f)
