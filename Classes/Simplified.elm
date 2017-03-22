module Classes.Simplified exposing (classSimplified)

import ModelDB exposing (..)
import FormsModel exposing (..)


classSimplified : Class
classSimplified =
    { name = "Simplified Class"
    , classPowerList = powers
    , classForms = forms
    , classPowerBlocks = \m -> []
    , modifyBasicMelee = Just simpleBasicMelee
    , modifyBasicRange = Just simpleBasicRange
    , modifyRally = Nothing
    , modifyCharge = Nothing
    , modifyHP = Nothing
    , classFeats = []
    }


powers : Model -> List Power
powers m =
    []  -- Well, they did say it was simplified..


simpleDamageEffect : Model -> String
simpleDamageEffect m =
    if (getLevel m) < 3 then
        "**Effect**: 1 damage."
    else if (getLevel m) < 7 then
        "**Effect**: 2 damage."
    else
        "**Effect**: 3 damage."

simpleDamageValue : Model -> Int
simpleDamageValue m =
    if (getLevel m) < 5 then
        2
    else if (getLevel m) < 9 then
        3
    else
        4

meleeSpecial : String
meleeSpecial =
    "\n\n**Special**: Treat any 2s rolled on the dice as though they were 5s."

simpleBasicMelee : Model -> Power -> Power
simpleBasicMelee m p =
    case (getResponse m "simple-type") of
        Just "Melee" ->
            { p | text = simpleDamageEffect m ++ meleeSpecial, damage = simpleDamageValue m }

        Just "None" ->
            { p | text = simpleDamageEffect m, damage = simpleDamageValue m }

        Nothing -> -- Assume no specialization if none is selected
            { p | text = simpleDamageEffect m, damage = simpleDamageValue m }

        _ ->
            p

simpleBasicRange : Model -> Power -> Power
simpleBasicRange m p =
    case (getResponse m "simple-type") of
        Just "Ranged" ->
            { p | text = simpleDamageEffect m, damage = simpleDamageValue m, range = 20 }

        Just "None" ->
            { p | text = simpleDamageEffect m, damage = simpleDamageValue m, range = 10 }

        Nothing -> -- Assume no specialization if none is selected
            { p | text = simpleDamageEffect m, damage = simpleDamageValue m, range = 10 }

        _ ->
            p

forms : Model -> List Form
forms m =
    [ Form False
        "Simplified Class"
        ([ DropdownField { name = "Specialization", del = False, key = "simple-type", choices = [ "", "None", "Melee", "Ranged" ] }
         ]
        )
    ]
