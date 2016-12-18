module Classes.Warlord exposing (classWarlord)

import ModelDB exposing (..)
import FormsModel exposing (..)
import PowerUtilities exposing (..)
import String


classWarlord : Class
classWarlord =
    { name = "Warlord"
    , classPowerList = powers
    , classForms = forms
    , classPowerBlocks = \m -> []
    , modifyBasicMelee = Just modifyBasicMelee
    , modifyBasicRange = Just modifyBasicRange
    , modifyRally = Nothing
    , modifyCharge = Nothing
    , modifyHP = Nothing
    , classFeats = []
    }


atWillDamage : Model -> Int
atWillDamage m =
    if (getLevel m < 5) then
        2
    else
        3


modifyBasicMelee : Model -> Power -> Power
modifyBasicMelee m p =
    { p | damage = atWillDamage m }


modifyBasicRange : Model -> Power -> Power
modifyBasicRange m p =
    { p | damage = atWillDamage m }


support =
    (levelTextSpecial "Support Tokens" [ 1, 5, 9 ])


rousing =
    (quickSpecial "Rousing")


enabling =
    (quickSpecial "Enabling")


incisive =
    (quickSpecial "Incisive")


specials m =
    powerDict m [ rousing, enabling, incisive ]


cspecial m =
    powerlookup m "warlord-special" specials


hitThisGuy m =
    levelTextPower "Hit This Thing" Attack AtWill 0 0 0 Green [ 1, 5, 9 ] m


alleyOop m =
    levelTextPower "Alley-Oop" Attack AtWill 0 0 -1 Green [ 1, 5, 9 ] m


punchingBag m =
    levelTextPower "Morale-Boosting Punching Bag" Attack AtWill -5 0 -1 Green [ 1, 5, 9 ] m


enumerate m =
    levelTextPower "Enumerate its Weaknesses" Attack AtWill -5 0 (atWillDamage m) Green [ 1, 9 ] m


offBalance m =
    levelTextPower "Knock Him Off Balance" Attack AtWill -5 0 (atWillDamage m) Green [ 1, 9 ] m


comeHelpMe m =
    levelTextPower "Come Help Me Over Here" Attack AtWill 0 0 0 Green [ 1, 5, 9 ] m


l1atwills m =
    powerDict m [ hitThisGuy, alleyOop, punchingBag, enumerate, offBalance, comeHelpMe ]


l1awchosen m =
    powerlookup m "warlord-aw1" l1atwills
        ++ powerlookup m "warlord-aw2" l1atwills
        ++ powerlookup m "warlord-aw3" l1atwills


myGrandma m =
    quickPower "You Hit Like a Baby" Attack Encounter 0 0 3 Purple m


perfectChance m =
    quickPower "The Perfect Chance" Attack Encounter 0 0 0 Purple m


defensiveTac m =
    quickPower "Defensive Tactics" Attack Encounter 0 0 0 Purple m


dontGiveUp m =
    quickPower "Don't Give Up" Reaction Encounter 0 0 0 Purple m


l1encounters m =
    powerDict m [ myGrandma, perfectChance, defensiveTac, dontGiveUp ]


l1echosen m =
    powerlookup m "warlord-enc1" l1encounters


neverSurrender m =
    quickPower "Never Surrender" Attack Encounter 0 0 0 Purple m


chanceToRecover m =
    quickPower "Chance to Recover" Misc Encounter 0 0 0 Purple m


leaveHimExposed m =
    quickPower "Leave Them Exposed" Attack Encounter -5 0 3 Purple m


l3encounters m =
    powerDict m [ neverSurrender, chanceToRecover, leaveHimExposed ]


l3echosen m =
    powerlookup m "warlord-enc3" l3encounters


battlecry m =
    case (getResponse m "warlord-special") of
        Nothing ->
            []

        Just x ->
            [ quickSpecial (x ++ " Battlecry") m ]


biggerBag m =
    quickPower "Bigger Punching Bag" Attack Encounter -5 0 -1 Purple m


leadCharge m =
    quickPower "Lead The Charge" Attack Encounter 0 0 0 Purple m


alwaysInPosition m =
    quickPower "Always in Position" Attack Encounter 0 0 4 Purple m


ultimatum m =
    quickPower "Ultimatum" Attack Encounter 0 0 0 Purple m


l7encounters m =
    powerDict m [ biggerBag, leadCharge, alwaysInPosition, ultimatum ]


l7echosen m =
    powerlookup m "warlord-enc7" l7encounters


eggpower m =
    case (getResponse m "basics-name") of
        Nothing ->
            []

        Just x ->
            if ((String.toLower x) == "master mike!") then
                [ Power "Wallace's Shout" "Shout at target creature. Its hand grows back." Attack AtWill 5 0 0 Green ]
            else
                []


powers m =
    [ support m ]
        ++ (cspecial m)
        ++ (l1awchosen m)
        ++ (l1echosen m)
        ++ (atLevelList m 3 (l3echosen m))
        ++ (atLevelList m 5 (battlecry m))
        ++ (atLevelList m 7 (l7echosen m))
        ++ eggpower m


forms m =
    [ Form False
        "Warlord"
        ([ powerChoiceField m "Feature:" "warlord-special" specials
         , powerChoiceField m "At-Will:" "warlord-aw1" l1atwills
         , powerChoiceField m "At-Will:" "warlord-aw2" l1atwills
         , powerChoiceField m "At-Will:" "warlord-aw3" l1atwills
         , powerChoiceField m "Encounter:" "warlord-enc1" l1encounters
         ]
            ++ (atLevel m 3 (powerChoiceField m "Encounter:" "warlord-enc3" l3encounters))
            ++ (atLevel m 7 (powerChoiceField m "Encounter:" "warlord-enc7" l7encounters))
        )
    ]
