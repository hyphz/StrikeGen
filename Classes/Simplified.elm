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


powers m =
    []


simpleDamageEffect m =
    if (getLevel m) < 3 then
        "**Effect**: 1 damage."
    else if (getLevel m) < 7 then
        "**Effect**: 2 damage."
    else
        "**Effect**: 3 damage."


simpleDamageValue m =
    if (getLevel m) < 5 then
        2
    else if (getLevel m) < 9 then
        3
    else
        4


meleeSpecial =
    "\n\n**Special**: Treat any 2s rolled on the dice as though they were 5s."


simpleBasicMelee m p =
    case (getResponse m "simple-type") of
        Just "Melee" ->
            { p | text = simpleDamageEffect m ++ meleeSpecial, damage = simpleDamageValue m }

        Just "None" ->
            { p | text = simpleDamageEffect m, damage = simpleDamageValue m }

        Nothing ->
            { p | text = simpleDamageEffect m, damage = simpleDamageValue m }

        _ ->
            p


simpleBasicRange m p =
    case (getResponse m "simple-type") of
        Just "Ranged" ->
            { p | text = simpleDamageEffect m, damage = simpleDamageValue m, range = 20 }

        Just "None" ->
            { p | text = simpleDamageEffect m, damage = simpleDamageValue m, range = 10 }

        Nothing ->
            { p | text = simpleDamageEffect m, damage = simpleDamageValue m, range = 10 }

        _ ->
            p


forms m =
    [ Form False
        "Simplified Class"
        ([ DropdownField { name = "Specialization", del = False, key = "simple-type", choices = [ "", "None", "Melee", "Ranged" ] }
         ]
        )
    ]
