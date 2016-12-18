module Roles.Defender exposing (roleDefender, mark)

import ModelDB exposing (..)
import FormsModel exposing (..)
import PowerUtilities exposing (..)


roleDefender : Role
roleDefender =
    { name = "Defender"
    , rolePowerList = (\m -> powers m "")
    , rolePowerListPrefix = powers
    , roleForms = (\m -> forms m "")
    , roleFormsPrefix = forms
    , modifySpeed = Nothing
    , roleFeats = [ "Distant Defender" ]
    }


defenseBoost m =
    if (getLevel m) < 4 then
        [ quickSpecial "Defense Boost" m ]
    else if (getLevel m) < 8 then
        [ quickSpecial "Improved Defense Boost" m ]
    else
        [ quickSpecial "Super Defense Boost" m ]


stickyBoost m =
    if (getLevel m) < 4 then
        [ quickSpecial "Stickiness Boost" m ]
    else if (getLevel m) < 8 then
        [ quickSpecial "Improved Stickiness Boost" m ]
    else
        [ quickSpecial "Super Stickiness Boost" m ]


defenderRangeModify m i =
    if (hasFeat m "Distant Defender") then
        i + 3
    else
        i


actionTrigger m =
    if (getLevel m) < 6 then
        [ quickPower "Call that a punch?" Reaction Encounter 0 0 0 Yellow m ]
    else
        [ quickPower "THIS is a punch!" Reaction Encounter 0 0 0 Yellow m ]


mark m =
    levelTextPower "Mark" RoleSlot AtWill (defenderRangeModify m 5) 0 0 Blue [ 1, 4, 8 ] m


boosts m =
    defenseBoost m
        ++ stickyBoost m
        ++ [ mark m ]


encounters m =
    powerDict m
        [ quickPower "I don't think so!" Reaction Encounter 0 0 0 Red
        , quickPower "Come and Get It!" RoleSlot Encounter 0 (defenderRangeModify m 2) 0 Red
        , quickPower "You're Mine!" RoleSlot Encounter 0 0 0 Red
        , quickPower "I'll Cover You!" Reaction Encounter 0 0 0 Red
        ]


upgraded x m =
    case x of
        "I don't think so!" ->
            [ quickPower "I really don't think so!" Reaction Encounter 0 0 0 Red m ]

        "Come and Get It!" ->
            [ quickPower "Come and Get Seconds!" RoleSlot Encounter 0 (defenderRangeModify m 3) 0 Red m ]

        "You're Mine!" ->
            [ quickPower "You're All Mine!" RoleSlot Encounter 0 0 0 Red m ]

        "I'll Cover You!" ->
            [ quickPower "I'll Cover You All!" Reaction Encounter 0 0 0 Red m ]

        _ ->
            []


checkUpgrade m p pr =
    if ((getLevel m) < 10) then
        p
    else
        case (List.head p) of
            Nothing ->
                p

            Just rp ->
                case (getResponse m (pr ++ "defender-upgrade")) of
                    Nothing ->
                        p

                    Just x ->
                        if (x == rp.name) then
                            upgraded rp.name m
                        else
                            p


l2encchosen m p =
    checkUpgrade m (prefixpowerlookup m p "defender-enc1" encounters) p


l6encchosen m p =
    checkUpgrade m (prefixpowerlookup m p "defender-enc2" encounters) p


upgradable m p =
    [ "" ]
        ++ (List.map .name
                (prefixpowerlookup m p "defender-enc1" encounters
                    ++ prefixpowerlookup m p "defender-enc2" encounters
                )
           )


powers m p =
    boosts m
        ++ actionTrigger m
        ++ atLevelList m 2 (l2encchosen m p)
        ++ atLevelList m 6 (l6encchosen m p)
        ++ if (hasFeat m "Distant Defender") then
            [ quickSpecial "Distant Defender" m ]
           else
            []


forms m p =
    [ Form False
        "Defender"
        (atLevel m 2 (prefixpowerChoiceField m "Encounter:" p "defender-enc1" encounters)
            ++ atLevel m 6 (prefixpowerChoiceField m "Encounter:" p "defender-enc2" encounters)
            ++ atLevel m 10 (DropdownField { name = "Upgrade:", del = False, key = (p ++ "defender-upgrade"), choices = (upgradable m p) })
        )
    ]
