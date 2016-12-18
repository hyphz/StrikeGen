module Classes.Rogue exposing (classRogue)

import ModelDB exposing (..)
import FormsModel exposing (..)
import PowerUtilities exposing (..)
import Dict


classRogue : Class
classRogue =
    { name = "Rogue"
    , classPowerList = powers
    , classForms = forms
    , classPowerBlocks = \m -> []
    , modifyBasicMelee = Just modifyBasicMelee
    , modifyBasicRange = Just modifyBasicRange
    , modifyRally = Nothing
    , modifyCharge = Nothing
    , modifyHP = Nothing
    , classFeats = ["Ambushing Rogue"]
    }


atWillDamage : Model -> Int
atWillDamage m =
    if ((getLevel m) < 5) then
        2
    else
        3


modifyBasicMelee : Model -> Power -> Power
modifyBasicMelee m p =
    { p | damage = atWillDamage m }


modifyBasicRange : Model -> Power -> Power
modifyBasicRange m p =
    { p | damage = atWillDamage m }


backstabber m = levelTextSpecial "Backstabber" [1,5,9] m

poisoner m = levelTextSpecial "Poisoner" [1,5,9] m

brawler m = levelTextSpecial "Brawler" [1,5,9] m

suckerPunch m = quickPower "Sucker Punch" Misc Encounter 0 0 0 Purple m
mindAltering m = quickPower "Mind Altering Poison" Attack Encounter 0 0 0 Purple m
calledShot m = quickPower "Called Shot" Attack Encounter 0 0 0 Purple m


specials m =
    Dict.fromList
        [ ( "Backstabber", backstabber m )
        , ( "Poisoner", poisoner m )
        , ( "Brawler", brawler m )
        ]

specialEncounter m =
    Dict.fromList
        [ ( "Backstabber", suckerPunch m )
        , ( "Poisoner", mindAltering m )
        , ( "Brawler", calledShot m )
        ]


cspecial m =
    (powerlookup m "rogue-feature" specials) ++
    (powerlookup m "rogue-feature" specialEncounter)


l1atwills m =
    powerDict m
        [ (quickPower "Sneak Attack" Move AtWill 0 0 0 Orange)
        , (quickPower "Hasten" Move AtWill 0 0 0 Orange)
        , (quickPower "Disengage" Move AtWill 0 0 0 Orange)
        , (quickPower "Flank" Move AtWill 0 0 0 Orange)
        , (quickPower "Trap" Move AtWill 0 0 0 Orange)
        ]


l1awchosen m =
          powerlookup m "rogue-aw1" l1atwills
        ++ powerlookup m "rogue-aw2" l1atwills
        ++ powerlookup m "rogue-aw3" l1atwills


l1encounters m =
    powerDict m
        [ (quickPower "Creep Off" Move Encounter 0 0 0 Cyan)
        , (quickPower "Evasion" Move Encounter 0 0 0 Cyan)
        , (quickPower "Throw" Move Encounter 0 0 0 Cyan)
        , (quickPower "Tactical Appraisal" Move Encounter 0 0 0 Cyan)
        ]


l1echosen m =
    powerlookup m "rogue-enc1" l1encounters


l3encounters m =
    powerDict m
        [ (quickPower "Smokescreen" Move Encounter 0 0 0 Cyan)
        , (quickPower "Vanish" Move Encounter 0 0 0 Cyan)
        , (quickPower "Path of Pain" Move Encounter 0 0 0 Cyan)
        , (quickPower "BAMF" Move Encounter 0 0 0 Cyan)
        , (quickPower "Lightning Speed" Move Encounter 0 0 0 Cyan)
        ]


l3echosen m =
    powerlookup m "rogue-enc3" l3encounters


l7encounters m =
    powerDict m
        [ (quickPower "Garotte" Attack Encounter 0 0 4 Purple)
        , (quickPower "Hamstring" Attack Encounter -5 0 4 Purple)
        , (quickPower "Knockout" Attack Encounter -5 0 4 Purple)
        , (quickPower "Throw Sand" Misc Encounter 0 0 0 Purple)
        ]


l7echosen m =
    powerlookup m "rogue-enc7" l7encounters


powers m = [quickSpecial "Rogue Copyright Warning" m] ++
           (cspecial m)
        ++ (l1awchosen m)
        ++ (l1echosen m)
        ++ (atLevelList m 3 (l3echosen m))
        ++ (atLevelList m 7 (l7echosen m))


forms m =
    [ Form False
        "Rogue"
        ([
           powerChoiceField m "Type:" "rogue-feature" specials
         , powerChoiceField m "At-Will:" "rogue-aw1" l1atwills
         , powerChoiceField m "At-Will:" "rogue-aw2" l1atwills
         , powerChoiceField m "At-Will:" "rogue-aw3" l1atwills
         , powerChoiceField m "Encounter:" "rogue-enc1" l1encounters
         ]
            ++ (atLevel m 3 (powerChoiceField m "Encounter:" "rogue-enc3" l3encounters))
            ++ (atLevel m 7 (powerChoiceField m "Encounter:" "rogue-enc7" l7encounters))
        )
    ]
