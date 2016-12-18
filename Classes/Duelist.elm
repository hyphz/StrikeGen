module Classes.Duelist exposing (classDuelist)

import ModelDB exposing (..)
import FormsModel exposing (..)
import PowerUtilities exposing (..)


classDuelist : Class
classDuelist =
    { name = "Duelist"
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


duel m =
    { name = "Duel"
    , slot = Misc
    , freq = Encounter
    , range = 0
    , area = 0
    , damage = 0
    , styl = Purple
    , text =
        (if (getLevel m) < 5 then
            overtext m "Duel"
         else if (getLevel m) < 9 then
            overtext m "Duel5+"
         else
            overtext m "Duel9+"
        )
    }


chargeTarget =
    quickPower "Change Target" Attack AtWill 0 0 0 Green


getOverHere m =
    (quickPower "Get Over Here" Attack AtWill 5 0 (atWillDamage m) Green) m


noEscape m =
    (quickPower "No Escape" Attack AtWill 0 0 (atWillDamage m) Green) m


demandAttention m =
    (quickPower "Demand Attention" Attack AtWill -5 0 (atWillDamage m) Green) m


exploitWeakness m =
    (quickPower "Exploit Weakness" Attack AtWill 0 0 (atWillDamage m) Green) m


guessingGame m =
    (quickPower "Guessing Game" Attack AtWill 0 0 (atWillDamage m) Green) m


noOneElse =
    quickPower "Ain't No One Else Around" Attack Encounter 0 0 3 Purple


perfectDefense =
    quickPower "Perfect Defense" Attack Encounter 0 0 3 Purple


takeTheOpening =
    quickPower "Take The Opening" Attack Encounter 0 0 3 Purple


stalker =
    quickPower "Stalker" Attack Encounter 0 0 3 Purple


cantIgnoreMe =
    quickPower "You Can't Ignore Me" Attack Encounter 0 0 4 Purple


takeThisOutside =
    quickPower "Let's Take This Outside" Attack Encounter 0 0 4 Purple


ifollow =
    quickPower "Where You Go I Follow" Attack Encounter 0 0 4 Purple


l1atwills m =
    powerDict m [ getOverHere, noEscape, demandAttention, exploitWeakness, guessingGame ]


l1awchosen m =
    powerlookup m "duelist-aw1" l1atwills
        ++ powerlookup m "duelist-aw2" l1atwills
        ++ powerlookup m "duelist-aw3" l1atwills


l1encounters m =
    powerDict m [ noOneElse, perfectDefense ]


l1echosen m =
    powerlookup m "duelist-enc1" l1encounters


l3encounters m =
    powerDict m [ takeTheOpening, stalker ]


l3encpower m =
    powerlookup m "duelist-enc3" l3encounters


l7encounters m =
    powerDict m [ cantIgnoreMe, takeThisOutside, ifollow ]


l7encpower m =
    powerlookup m "duelist-enc7" l7encounters


specials m =
    case (getResponse m "duelist-feature") of
        Just s ->
            [ quickSpecial s m ]

        Nothing ->
            []


focus m =
    { name = "Focus"
    , text =
        if (getLevel m) < 5 then
            overtext m "Focus"
        else if (getLevel m) < 9 then
            overtext m "Focus5+"
        else
            overtext m "Focus9+"
    , slot = Special
    , freq = None
    , damage = 0
    , area = 0
    , range = 0
    , styl = White
    }


powers m =
    [ focus m ]
        ++ specials m
        ++ [ duel m, chargeTarget m ]
        ++ l1awchosen m
        ++ l1echosen m
        ++ (atLevelList m 3 (l3encpower m))
        ++ (atLevelList m 7 (l7encpower m))


forms m =
    [ Form False
        "Duelist"
        ([ DropdownField { name = "Feature", del = False, key = "duelist-feature", choices = [ "", "Find an Opening", "Throw off their Aim", "Force their position" ] }
         , powerChoiceField m "At-Will:" "duelist-aw1" l1atwills
         , powerChoiceField m "At-Will:" "duelist-aw2" l1atwills
         , powerChoiceField m "At-Will:" "duelist-aw3" l1atwills
         , powerChoiceField m "Encounter:" "duelist-enc1" l1encounters
         ]
            ++ (atLevel m 3 (powerChoiceField m "Encounter:" "duelist-enc3" l3encounters))
            ++ (atLevel m 7 (powerChoiceField m "Encounter:" "duelist-enc7" l7encounters))
        )
    ]
