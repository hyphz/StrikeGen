module Classes.MartialArtist exposing (classMA)

import ModelDB exposing (..)
import FormsModel exposing (..)
import PowerUtilities exposing (..)
import String

classMA : Class
classMA = { name = "Duelist",
               classPowerList = powers,
               classForms = forms,
               classPowerBlocks = \m -> [],
               modifyBasicMelee = Just modifyBasicMelee,
               modifyBasicRange = Just modifyBasicRange,
               modifyRally = Nothing,
               modifyCharge = Nothing }

atWillDamage : Model -> Int
atWillDamage m = if (getLevel m < 5) then 2 else 3

modifyBasicMelee : Model -> Power -> Power
modifyBasicMelee m p = {p | damage = atWillDamage m}

modifyBasicRange : Model -> Power -> Power
modifyBasicRange m p = {p | damage = atWillDamage m}

startStance = quickSpecial "Stances"
changeStance = quickPower "Change Stance" Special AtWill 0 0 0 Yellow
focusedAttack = quickPower "Focused Attack" Special Encounter 0 0 0 Purple
quickStance m n = quickPower n Special None 0 0 0 Yellow m

stanceList = ["Stone Wall","Weeping Willow","Tempest","Python","Scorpion",
          "Flickering Flame","Mandala"]

decapitation = quickPower "Decapitation" Attack Encounter 0 0 3 Purple
theFalseDeath = quickPower "The False Death" Special Encounter 0 0 0 Purple
martialFrenzy = quickPower "Martial Frenzy" Special Encounter 0 0 0 Purple
extraFocusedAttack = quickPower "Extra Focused Attack" Special Encounter 0 0 0 Purple

genStances m =
  let
    stanceChoices = mayList (getResponse m "ma-s1") ++
                    mayList (getResponse m "ma-s2") ++
                    mayList (getResponse m "ma-s3")
    allStances x = [quickStance m (x ++ " Style"),
                    quickStance m ("Greater "++x++ " Style"),
                    quickStance m ("Supreme "++x++ " Style")]
  in (List.concatMap (allStances) stanceChoices)

l3encounters m = powerDict m [extraFocusedAttack, decapitation, theFalseDeath]
l3encpower m = powerlookup m "ma-enc3" l3encounters

l7encounters m = powerDict m [extraFocusedAttack, decapitation, theFalseDeath, martialFrenzy]
l7encpower m = powerlookup m "ma-enc7" l7encounters

mamaster m = quickSpecial "Master Martial Artist" m

eggpower m = case (getResponse m "basics-name") of
  Nothing -> []
  Just x -> if ((String.toLower x) == "daniel-san") then
    [Power "I've been your servant" "**Effect:** Choose one: remove all wax from target vehicle, sand target floor or fence, paint target house." Attack AtWill 0 0 0 Green]
  else []


powers m = genStances m ++ [startStance m, changeStance m, focusedAttack m]
           ++ (atLevelList m 3 (l3encpower m))
           ++ (atLevelList m 7 (l7encpower m))
           ++ (atLevel m 9 (mamaster m))
           ++ eggpower m

forms m = [Form False "Martial Artist" ([
  DropdownField { name = "Stance:", key="ma-s1", choices=[""] ++ stanceList, del=False },
  DropdownField { name = "Stance:", key="ma-s2", choices=[""] ++ stanceList, del=False },
  DropdownField { name = "Stance:", key="ma-s3", choices=[""] ++ stanceList, del=False }]
  ++ (atLevel m 3 (powerChoiceField m "Encounter:" "ma-enc3" l3encounters))
  ++ (atLevel m 7 (powerChoiceField m "Encounter:" "ma-enc7" l7encounters))
  )]
