module Classes.Magician exposing (classMagician)

import ModelDB exposing (..)
import FormsModel exposing (..)
import Dict exposing (..)
import PowerUtilities exposing (..)


classMagician : Class
classMagician = { name = "Magician",
               classPowerList = powers,
               classForms = forms,
               classPowerBlocks = powerBlocks,
               modifyBasicMelee = Just modifyBasicMelee,
               modifyBasicRange = Just modifyBasicRange,
               modifyRally = Just modifyRally,
               modifyCharge = Nothing }


atWillDamage : Model -> Int
atWillDamage m = if (getLevel m < 5) then 2 else 3

modifyBasicMelee : Model -> Power -> Power
modifyBasicMelee m p = {p | damage = atWillDamage m}

modifyBasicRange : Model -> Power -> Power
modifyBasicRange m p = {p | damage = atWillDamage m, range = 10 }

modifyRally : Model -> Power -> Power
modifyRally m p = {p | text = overtext m "MagicianRally" }


repulsion m = quickPower "The Instant Repulsion" Attack AtWill 0 0 (atWillDamage m) Green m
lubrication m = quickPower "Liscato's Bountiful Lubrication" Attack AtWill 10 0 (atWillDamage m) Green m
marguls m = levelTextPower "Margul's Toxic Missile" Attack AtWill 10 0 (atWillDamage m) Green [1,5] m
prismatic = levelTextPower "The Excellent Prismatic Spray" Attack AtWill 10 0 0 Green [1,5]
awspells m = powerDict m [repulsion, lubrication, marguls, prismatic]
chosenaws m = powerlookup m "mage-aw1" awspells ++ powerlookup m "mage-aw2" awspells

friendship = quickPower "Word of Instant Friendship" Attack Encounter 10 0 3 Purple
encystment = quickPower "Momentary Encystment" Attack Encounter 10 0 3 Purple
command = quickPower "Furz's Undeniable Command" Attack Encounter 10 0 3 Purple
soporific = quickPower "Soporific Decree" Attack Encounter 10 0 3 Purple
constriction = quickPower "Parvel's Total Constriction" Attack Encounter 10 0 3 Purple
motes = quickPower "Radiant Motes of the Overworld" Attack Encounter 10 0 3 Purple
ripening = quickPower "Hura's Hasty Ripening" Attack Encounter 10 0 3 Purple
caltrop = quickPower "Hozrel's Efficacious Caltrop" Attack Encounter 0 0 3 Purple
cant = quickPower "The Cant of Inexplicable Lust" Attack Encounter 10 0 3 Purple
dome = quickPower "Practical Dome of Preservation" Reaction Encounter 0 0 0 Purple
inferno = quickPower "Mudge's Localized Inferno" Attack Encounter 0 0 0 Purple
amalgam = quickPower "Horwell's Offensive Amalgam" Attack Encounter 10 0 0 Purple
encspells m = powerDict m [friendship, encystment, command, soporific, constriction,
                           motes, ripening, caltrop, cant, dome, inferno, amalgam]

enclookup m k = powerlookup m k encspells

encasement = quickPower "The Otherworldly Encasement" Attack Encounter 10 0 5 Yellow
malediction = quickPower "Thanatin's Malediction" Attack Encounter 10 0 5 Yellow
subjugation = quickPower "Fay Zu's Lesser Subjugation" Attack Encounter 10 0 5 Yellow
colors = quickPower "The Dome of Many Colors" Attack Encounter 10 0 5 Yellow
vulnerable = quickPower "Incantation of Vulnerability" Attack Encounter 10 0 5 Yellow
slowmoment = quickPower "Renalai's Slow Moment" Attack Encounter 10 0 5 Yellow
majspells m = powerDict m [encasement, malediction, subjugation, colors, vulnerable,
  slowmoment]
majlookup m k = powerlookup m k majspells

decorate s p =
  { p | name = "(" ++ s ++ ") " ++ p.name }

starPowers m =
    [levelTextSpecial "Star Magic" [1, 5, 7] m] ++
    enclookup m "mage-enc1" ++ enclookup m "mage-enc2" ++ enclookup m "mage-enc3"
    ++ atLevelList m 3 (enclookup m "mage-enc4" ++ enclookup m "mage-enc5" ++ enclookup m "mage-enc6")
    ++ atLevelList m 5 (majlookup m "mage-maj1")
    ++ atLevelList m 7 (enclookup m "mage-enc7" ++ majlookup m "mage-maj2")
    ++ atLevelList m 9 (majlookup m "mage-maj3")


chaosEncounterKeys m =
  ["mage-enc1","mage-enc2","mage-enc3"]
  ++ atLevelList m 3 ["mage-enc4","mage-enc5","mage-enc6"]

chaosMajorKeys m =
   atLevel m 5 "mage-maj1"
    ++ atLevel m 7 "mage-maj2"
    ++ atLevel m 9 "mage-maj3"

reserveBlock m =
  if (((getResponse m "mage-source") == Just "Star") && ((getLevel m) >= 9)) then
    {name="Reserve Spell",powers=majlookup m "mage-mres"}
  else
    {name="Reserve Spell",powers=enclookup m "mage-res"}


powerBlocks m =
  [reserveBlock m] ++
    (if (getResponse m "mage-source") == Just "Chaos" then [chaosSurgeBlock m] else []) ++
    (if (getResponse m "mage-source") == Just "Blood" then bloodCurseBlock m else [])

chaosSurges m chosenkeys mainlist =
  let
    slist = Dict.keys mainlist
    choices = List.map (getResponse m) chosenkeys
    unpickedkeys = List.filter (\x -> not (List.member (Just x) choices)) slist
    unpickedspells = List.concatMap (\x -> (mayList (Dict.get x mainlist))) unpickedkeys
  in
    unpickedspells

chaosSurgeBlock m =
       if ((getLevel m) < 9) then
         {name="Chaos Surges", powers=chaosSurges m ((chaosEncounterKeys m) ++ ["mage-res"]) (encspells m)}
       else
         {name="Chaos Surges", powers=chaosSurges m (chaosMajorKeys m) (majspells m)}

chaosPowers m =
    [levelTextSpecial "Chaos Magic" [1, 3, 5, 7, 9] m] ++
    List.concatMap (enclookup m) ((chaosEncounterKeys m)) ++
    List.concatMap (majlookup m) (chaosMajorKeys m)

bloodCurseBlock m =
  if ((getLevel m) < 5) then []
    else [{name="Blood Curse",powers=majlookup m "mage-bc"}]



bloodPowers m =
    [levelTextSpecial "Blood Magic" [1, 5, 7, 9] m] ++
    enclookup m "mage-enc1"
    ++ atLevelList m 3 (enclookup m "mage-enc2")
    ++ atLevelList m 7 (enclookup m "mage-maj1")




starForm m =
  [powerChoiceField m "At-Will:" "mage-aw1" awspells,
   powerChoiceField m "At-Will:" "mage-aw2" awspells,
   powerChoiceField m "Encounter:" "mage-enc1" encspells,
   powerChoiceField m "Encounter:" "mage-enc2" encspells,
   powerChoiceField m "Encounter:" "mage-enc3" encspells]
  ++ atLevelList m 3 [powerChoiceField m "Encounter:" "mage-enc4" encspells,
                      powerChoiceField m "Encounter:" "mage-enc5" encspells,
                      powerChoiceField m "Encounter:" "mage-enc6" encspells]
  ++ atLevelList m 7 [powerChoiceField m "Encounter:" "mage-enc7" encspells]
  ++ atLevelList m 5 [powerChoiceField m "Major:" "mage-maj1" majspells]
  ++ atLevelList m 7 [powerChoiceField m "Major:" "mage-maj2" majspells]
  ++ atLevelList m 9 [powerChoiceField m "Major:" "mage-maj3" majspells]
  ++ (if ((getLevel m) < 9) then
        [powerChoiceField m "Reserve:" "mage-res" encspells]
      else
        [powerChoiceField m "Reserve:" "mage-mres" majspells])

chaosForm m =
  [powerChoiceField m "At-Will:" "mage-aw1" awspells,
   powerChoiceField m "At-Will:" "mage-aw2" awspells,
   powerChoiceField m "Encounter:" "mage-enc1" encspells,
   powerChoiceField m "Encounter:" "mage-enc2" encspells,
   powerChoiceField m "Encounter:" "mage-enc3" encspells]
  ++ atLevelList m 3 [powerChoiceField m "Encounter:" "mage-enc4" encspells,
                      powerChoiceField m "Encounter:" "mage-enc5" encspells,
                      powerChoiceField m "Encounter:" "mage-enc6" encspells]
  ++ atLevelList m 5 [powerChoiceField m "Major:" "mage-maj1" majspells]
  ++ atLevelList m 7 [powerChoiceField m "Major:" "mage-maj2" majspells]
  ++ atLevelList m 9 [powerChoiceField m "Major:" "mage-maj3" majspells]
  ++ [powerChoiceField m "Reserve:" "mage-res" encspells]

bloodForm m =
  [powerChoiceField m "At-Will:" "mage-aw1" awspells,
   powerChoiceField m "At-Will:" "mage-aw2" awspells,
   powerChoiceField m "Encounter:" "mage-enc1" encspells]
  ++ atLevelList m 3 [powerChoiceField m "Encounter:" "mage-enc2" encspells]
  ++ atLevelList m 5 [powerChoiceField m "Blood Curse:" "mage-bc" majspells]
  ++ atLevelList m 7 [powerChoiceField m "Major:" "mage-maj1" majspells]
  ++ [powerChoiceField m "Reserve:" "mage-res" encspells]


powers m = chosenaws m ++ (case (getResponse m "mage-source") of
                             Just "Star" -> starPowers m
                             Just "Chaos" -> chaosPowers m
                             Just "Blood" -> bloodPowers m
                             _ -> [])





forms m = [Form False "Magician" ([
  DropdownField { name = "Source:", key = "mage-source", choices=["","Star","Chaos","Blood"], del=False }]
  ++ (case (getResponse m "mage-source") of
        Just "Star"-> starForm m
        Just "Chaos" -> chaosForm m
        Just "Blood" -> bloodForm m
        _ -> []
     ))]
