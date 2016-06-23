module Archer exposing (classArcher)

import ModelDB exposing (..)
import FormsModel exposing (..)
import Dict exposing (..)
import PowerUtilities exposing (..)


classArcher : Class
classArcher = { name = "Archer",
               classPowerList = powers,
               classForms = forms,
               modifyBasicMeleeDamage = basicMeleeDamage,
               modifyBasicRangeDamage = basicRangeDamage,
               modifyBasicRangeRange = basicRangeRange }

sniperDouble : Model -> Int -> Int
sniperDouble m i = case (getResponse m "archer-feature") of
  Just "Sniper" -> i * 2
  _ -> i

basicMeleeDamage : Model -> Int
basicMeleeDamage m = if (getLevel m < 5) then 0 else 1

basicRangeDamage : Model -> Int
basicRangeDamage m = if (getLevel m < 5) then 0 else 1

atWillDamage : Model -> Int
atWillDamage m = if (getLevel m < 5) then 2 else 3

basicRangeRange m = case (getResponse m "archer-feature") of
  Just "Sniper" -> 15
  _ -> 5



aim = quickPower "Aim" 106 Attack AtWill 0 0 0 Green
flare m = (quickPower "Flare" 106 Attack AtWill (sniperDouble m 10) 0 (atWillDamage m) Green) m
pinDown m = (quickPower "Pin Down" 106 Attack AtWill (sniperDouble m 10) 0 (atWillDamage m) Green) m
areaDenial m = (quickPower "Area Denial" 106 Attack AtWill (sniperDouble m 10) 0 (atWillDamage m) Green) m

trickArrow m = (quickPower "Trick Arrow" 106 Attack Encounter (sniperDouble m 10) 0 3 Purple) m

bullseye = (quickPower "Bullseye" 107 Attack Encounter 0 0 0 Purple)

extraTrickArrow m = (quickPower "Extra Trick Arrow" 107 Attack Encounter (sniperDouble m 10) 0 3 Purple) m


powers m = [aim m, flare m, pinDown m, areaDenial m]


forms m = [Form False "Archer" ([
  DropdownField { name="Feature", del=False, key="archer-feature", choices=["","Sniper","Blitzer","Sentinel"] }
  ])]
