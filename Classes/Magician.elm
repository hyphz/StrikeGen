module Classes.Magician exposing (classMagician)

import ModelDB exposing (..)
import FormsModel exposing (..)
import Dict exposing (..)
import PowerUtilities exposing (..)
import String


classMagician : Class
classMagician =
    { name = "Magician"
    , classPowerList = powers
    , classForms = forms
    , classPowerBlocks = powerBlocks
    , modifyBasicMelee = Just modifyBasicMelee
    , modifyBasicRange = Just modifyBasicRange
    , modifyRally = Just modifyRally
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
    { p | damage = atWillDamage m, range = 10 }


modifyRally : Model -> Power -> Power
modifyRally m p =
    { p | text = overtext m "MagicianRally" }


-- The available at-will spells.

repulsion : Model -> Power
repulsion m =
    quickPower "The Instant Repulsion" Attack AtWill 0 0 (atWillDamage m) Green m


lubrication : Model -> Power
lubrication m =
    quickPower "Liscato's Bountiful Lubrication" Attack AtWill 10 0 (atWillDamage m) Green m

marguls : Model -> Power
marguls m =
    levelTextPower "Margul's Toxic Missile" Attack AtWill 10 0 (atWillDamage m) Green [ 1 ] m

prismatic : Model -> Power
prismatic =
    levelTextPower "The Excellent Prismatic Spray" Attack AtWill 10 0 0 Green [ 1, 5 ]

awspells : Model -> Dict String Power
awspells m =
    powerDict m [ repulsion, lubrication, marguls, prismatic ]

{-| Returns the list of chosen at-will spells in the model.
-}
chosenaws : Model -> List Power
chosenaws m =
    powerlookup m "mage-aw1" awspells
        ++ powerlookup m "mage-aw2" awspells
        ++ powerlookup m "mage-aw3" awspells


-- The available encounter spells.

friendship : Model -> Power
friendship =
    quickPower "Word of Instant Friendship" Attack Encounter 10 0 3 Purple

encystment : Model -> Power
encystment =
    quickPower "Momentary Encystment" Attack Encounter 10 0 3 Purple

command : Model -> Power
command =
    quickPower "Furz's Undeniable Command" Attack Encounter 10 0 3 Purple

soporific : Model -> Power
soporific =
    quickPower "Soporific Decree" Attack Encounter 10 0 3 Purple

constriction : Model -> Power
constriction =
    quickPower "Parvel's Total Constriction" Attack Encounter 10 0 3 Purple

motes : Model -> Power
motes =
    quickPower "Radiant Motes of the Overworld" Attack Encounter 10 0 3 Purple

ripening : Model -> Power
ripening =
    quickPower "Hura's Hasty Ripening" Attack Encounter 10 0 3 Purple

caltrop : Model -> Power
caltrop =
    quickPower "Hozrel's Efficacious Caltrop" Attack Encounter 0 0 3 Purple

cant : Model -> Power
cant =
    quickPower "The Cant of Inexplicable Lust" Attack Encounter 10 0 3 Purple

dome : Model -> Power
dome =
    quickPower "Practical Dome of Preservation" Reaction Encounter 0 0 0 Purple

inferno : Model -> Power
inferno =
    quickPower "Mudge's Localized Inferno" Attack Encounter 0 0 0 Purple

amalgam : Model -> Power
amalgam =
    quickPower "Horwell's Offensive Amalgam" Attack Encounter 10 0 0 Purple

encspells : Model -> Dict String Power
encspells m =
    powerDict m
        [ friendship
        , encystment
        , command
        , soporific
        , constriction
        , motes
        , ripening
        , caltrop
        , cant
        , dome
        , inferno
        , amalgam
        ]

{-| Look up an encounter spell by name.
-}
enclookup : Model -> String -> List Power
enclookup m k =
    powerlookup m k encspells

-- Major spells.

encasement : Model -> Power
encasement =
    quickPower "The Otherworldly Encasement" Attack Encounter 10 0 5 Yellow

malediction : Model -> Power
malediction =
    quickPower "Thanatin's Malediction" Attack Encounter 10 0 5 Yellow

subjugation : Model -> Power
subjugation =
    quickPower "Fay Zu's Lesser Subjugation" Attack Encounter 10 0 5 Yellow

colors : Model -> Power
colors =
    quickPower "The Dome of Many Colors" Attack Encounter 10 0 5 Yellow

vulnerable : Model -> Power
vulnerable =
    quickPower "Incantation of Vulnerability" Attack Encounter 10 0 5 Yellow

slowmoment : Model -> Power
slowmoment =
    quickPower "Renalai's Slow Moment" Attack Encounter 10 0 5 Yellow

majspells : Model -> Dict String Power
majspells m =
    powerDict m
        [ encasement
        , malediction
        , subjugation
        , colors
        , vulnerable
        , slowmoment
        ]

{-| Looks up a major spell by name.
-}
majlookup : Model -> String -> List Power
majlookup m k =
    powerlookup m k majspells

{-| Decorates a power's name with a given string.
-}
decorate : String -> Power -> Power
decorate s p =
    { p | name = "(" ++ s ++ ") " ++ p.name }


{-| Power rules for star mages.
-}
starPowers : Model -> List Power
starPowers m =
    [ levelTextSpecial "Star Magic" [ 1, 5, 7 ] m ]
        ++ enclookup m "mage-enc1"
        ++ enclookup m "mage-enc2"
        ++ enclookup m "mage-enc3"
        ++ atLevelList m 3 (enclookup m "mage-enc4" ++ enclookup m "mage-enc5" ++ enclookup m "mage-enc6")
        ++ atLevelList m 5 (majlookup m "mage-maj1")
        ++ atLevelList m 7 (enclookup m "mage-enc7" ++ majlookup m "mage-maj2")
        ++ atLevelList m 9 (majlookup m "mage-maj3")

{-| Range of possible keys for chaos rolls.
-}
chaosEncounterKeys : Model -> List String
chaosEncounterKeys m =
    [ "mage-enc1", "mage-enc2", "mage-enc3" ]
        ++ atLevelList m 3 [ "mage-enc4", "mage-enc5", "mage-enc6" ]

chaosMajorKeys : Model -> List String
chaosMajorKeys m =
    atLevel m 5 "mage-maj1"
        ++ atLevel m 7 "mage-maj2"
        ++ atLevel m 9 "mage-maj3"

{-| Block holding the single reserve spell. Special case for a level 9+ Star Mage
because they get a reserve Major spell. -}
reserveBlock : Model -> PowerBlock
reserveBlock m =
    if (((getResponse m "mage-source") == Just "Star") && ((getLevel m) >= 9)) then
        { name = "Reserve Spell", powers = majlookup m "mage-mres" }
    else
        { name = "Reserve Spell", powers = enclookup m "mage-res" }

{-| Additional power blocks. Everyone gets a reserve spell, chaos mages get chaos
surges, blood mages get their blood curse. -}
powerBlocks : Model -> List PowerBlock
powerBlocks m =
    [ reserveBlock m ]
        ++ (if (getResponse m "mage-source") == Just "Chaos" then
                [ chaosSurgeBlock m ]
            else
                []
           )
        ++ (if (getResponse m "mage-source") == Just "Blood" then
                bloodCurseBlock m
            else
                []
           )

{-| Calculates all powers available to a chaos mage as chaos surges. -}
chaosSurges : Model -> List String -> Dict String Power -> List Power
chaosSurges m chosenkeys mainlist =
    let
        slist =                   -- List of all possible spell keys in specified type.
            Dict.keys mainlist

        choices =                 -- List of all selected spell keys in specified type.
            List.map (getResponse m) chosenkeys

        unpickedkeys =            -- List of all spell keys in slist but not in choices.
            List.filter (\x -> not (List.member (Just x) choices)) slist

        unpickedspells =          -- List of all spells corresponding to those keys.
            List.concatMap (\x -> (mayList (Dict.get x mainlist))) unpickedkeys
    in
        unpickedspells


{-| Calculates chaos surge block - encounter level below level 9, major at and above 9. -}

chaosSurgeBlock : Model -> PowerBlock
chaosSurgeBlock m =
    if ((getLevel m) < 9) then
        { name = "Chaos Surges", powers = chaosSurges m ((chaosEncounterKeys m) ++ [ "mage-res" ]) (encspells m) }
    else
        { name = "Chaos Surges", powers = chaosSurges m (chaosMajorKeys m) (majspells m) }


-- Regular (selected) powers of a chaos mage.
chaosPowers : Model -> List Power
chaosPowers m =
    [ levelTextSpecial "Chaos Magic" [ 1, 3, 5, 7, 9 ] m ]
        ++ List.concatMap (enclookup m) ((chaosEncounterKeys m))
        ++ List.concatMap (majlookup m) (chaosMajorKeys m)


-- Blood curse block (just one power) for a blood mage.
bloodCurseBlock : Model -> List PowerBlock
bloodCurseBlock m =
    if ((getLevel m) < 5) then
        []
    else
        [ { name = "Blood Curse", powers = majlookup m "mage-bc" } ]

-- Regular powers for a blood mage.
bloodPowers : Model -> List Power
bloodPowers m =
    [ levelTextSpecial "Blood Magic" [ 1, 5, 7, 9 ] m ]
        ++ enclookup m "mage-enc1"
        ++ atLevelList m 3 (enclookup m "mage-enc2")
        ++ atLevelList m 7 (majlookup m "mage-maj1")

-- Part of form for star mage powers.
starForm : Model -> List Field
starForm m =
    [ powerChoiceField m "At-Will:" "mage-aw1" awspells
    , powerChoiceField m "At-Will:" "mage-aw2" awspells
    , powerChoiceField m "At-Will:" "mage-aw3" awspells
    , powerChoiceField m "Encounter:" "mage-enc1" encspells
    , powerChoiceField m "Encounter:" "mage-enc2" encspells
    , powerChoiceField m "Encounter:" "mage-enc3" encspells
    ]
        ++ atLevelList m
            3
            [ powerChoiceField m "Encounter:" "mage-enc4" encspells
            , powerChoiceField m "Encounter:" "mage-enc5" encspells
            , powerChoiceField m "Encounter:" "mage-enc6" encspells
            ]
        ++ atLevelList m 7 [ powerChoiceField m "Encounter:" "mage-enc7" encspells ]
        ++ atLevelList m 5 [ powerChoiceField m "Major:" "mage-maj1" majspells ]
        ++ atLevelList m 7 [ powerChoiceField m "Major:" "mage-maj2" majspells ]
        ++ atLevelList m 9 [ powerChoiceField m "Major:" "mage-maj3" majspells ]
        ++ (if ((getLevel m) < 9) then
                [ powerChoiceField m "Reserve:" "mage-res" encspells ]
            else
                [ powerChoiceField m "Reserve:" "mage-mres" majspells ]
           )

-- Part of form for chaos mage powers.
chaosForm : Model -> List Field
chaosForm m =
    [ powerChoiceField m "At-Will:" "mage-aw1" awspells
    , powerChoiceField m "At-Will:" "mage-aw2" awspells
    , powerChoiceField m "Encounter:" "mage-enc1" encspells
    , powerChoiceField m "Encounter:" "mage-enc2" encspells
    , powerChoiceField m "Encounter:" "mage-enc3" encspells
    ]
        ++ atLevelList m
            3
            [ powerChoiceField m "Encounter:" "mage-enc4" encspells
            , powerChoiceField m "Encounter:" "mage-enc5" encspells
            , powerChoiceField m "Encounter:" "mage-enc6" encspells
            ]
        ++ atLevelList m 5 [ powerChoiceField m "Major:" "mage-maj1" majspells ]
        ++ atLevelList m 7 [ powerChoiceField m "Major:" "mage-maj2" majspells ]
        ++ atLevelList m 9 [ powerChoiceField m "Major:" "mage-maj3" majspells ]
        ++ [ powerChoiceField m "Reserve:" "mage-res" encspells ]

-- Part of form for blood mage powers.
bloodForm : Model -> List Field
bloodForm m =
    [ powerChoiceField m "At-Will:" "mage-aw1" awspells
    , powerChoiceField m "At-Will:" "mage-aw2" awspells
    , powerChoiceField m "Encounter:" "mage-enc1" encspells
    ]
        ++ atLevelList m 3 [ powerChoiceField m "Encounter:" "mage-enc2" encspells ]
        ++ atLevelList m 5 [ powerChoiceField m "Blood Curse:" "mage-bc" majspells ]
        ++ atLevelList m 7 [ powerChoiceField m "Major:" "mage-maj1" majspells ]
        ++ [ powerChoiceField m "Reserve:" "mage-res" encspells ]

-- Easter egg power.
eggpower : Model -> List Power
eggpower m =
    case (getResponse m "basics-name") of
        Nothing ->
            []

        Just x ->
            if ((String.toLower x) == "master mike!") then
                [ Power "Game Balance" "All your enemies are immediately Taken Out. All team and individual Strikes accumulated by your allies are removed. After using this power, you fall asleep for 8 hours." Attack Encounter 0 0 0 Purple ]
            else
                []

-- Overall powers function, selecting the appropriate rule set based on source.
powers : Model -> List Power
powers m =
    chosenaws m
        ++ (case (getResponse m "mage-source") of
                Just "Star" ->
                    starPowers m

                Just "Chaos" ->
                    chaosPowers m

                Just "Blood" ->
                    bloodPowers m

                _ ->
                    []
           )
        ++ eggpower m

-- Overall form function - fixed source field, plus remainder of form based on selected source.
forms : Model -> List Form
forms m =
    [ Form False
        "Magician"
        ([ DropdownField { name = "Source:", key = "mage-source", choices = [ "", "Star", "Chaos", "Blood" ], del = False }
         ]
            ++ (case (getResponse m "mage-source") of
                    Just "Star" ->
                        starForm m

                    Just "Chaos" ->
                        chaosForm m

                    Just "Blood" ->
                        bloodForm m

                    _ ->
                        []
               )
        )
    ]
