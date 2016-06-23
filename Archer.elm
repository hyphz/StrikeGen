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
pinDown m = {name = "Pin Down",
           text = if (getLevel m) < 5 then overtext m "PinDown" "See page 106." else
                  if (getLevel m) < 9 then overtext m "PinDown5+" "See page 106." else
                    overtext m "PinDown9+" "See page 106.",
          slot=Attack, freq=AtWill,
          range=(sniperDouble m 10), area=0, damage=(atWillDamage m), styl=Green}

areaDenial m = {name = "Area Denial",
           text = if (getLevel m) < 5 then overtext m "AreaDenial" "See page 106." else
                  if (getLevel m) < 9 then overtext m "AreaDenial5+" "See page 106." else
                    overtext m "AreaDenial9+" "See page 106.",
          slot=Attack, freq=AtWill,
          range=(sniperDouble m 10), area=0, damage=(atWillDamage m), styl=Green}

trickArrow m = (quickPower "Trick Arrow" 106 Attack Encounter (sniperDouble m 10) 0 3 Purple) m

bullseye = (quickPower "Bullseye" 107 Attack Encounter 0 0 0 Purple)

extraTrickArrow m = (quickSpecial "Extra Trick Arrow" 107 m)
legShot m = (quickPower "Leg Shot" 107 Attack Encounter (sniperDouble m 10) 0 3 Purple) m
surprisingShot m = (quickPower "Surprising Shot" 107 Attack Encounter (sniperDouble m 10) 0 3 Purple) m
splitTheirArrow = quickPower "Split Their Arrow" 107 Reaction Encounter 0 0 0 Purple


youCantHide = quickPower "You Can't Hide" 107 Attack Encounter 0 0 0 Purple
superTrickArrow m = (quickPower "Super Trick Arrow" 107 Attack Encounter (sniperDouble m 10) 0 4 Purple) m

l1encounters m = powerDict m [trickArrow, bullseye]
l1encpower m = powerlookup m "archer-enc1" l1encounters

l3encounters m = powerDict m [extraTrickArrow, legShot, surprisingShot, splitTheirArrow]
l3encpower m = powerlookup m "archer-enc3" l3encounters

l7encounters m = powerDict m [youCantHide, superTrickArrow]
l7encpower m = powerlookup m "archer-enc7" l7encounters

specials m = case (getResponse m "archer-feature") of
  Just "Sniper" -> [quickSpecial "Sniper" 107 m] ++
                   atLevel m 5 (quickSpecial "Steady Sniper" 107 m) ++
                   atLevel m 9 (quickSpecial "Sharpshooting Sniper" 107 m)
  Just "Blitzer" -> [quickSpecial "Blitzer" 107 m] ++
                   atLevel m 5 (quickSpecial "Bloody Blitzer" 107 m) ++
                   atLevel m 9 (quickSpecial "Lightning Blitzer" 107 m)
  Just "Sentinel" -> if (getLevel m < 5) then [quickSpecial "Sentinel" 107 m] else [] ++
                   atLevel m 5 (quickSpecial "Sharp Sentinel" 107 m) ++
                   atLevel m 9 (quickSpecial "Snapshot Sentinel" 107 m)

  _ -> []



powers m = specials m ++ [aim m, flare m, pinDown m, areaDenial m]
           ++ l1encpower m
           ++ (atLevelList m 3 (l3encpower m))
           ++ (atLevelList m 7 (l7encpower m))




forms m = [Form False "Archer" ([
  DropdownField { name="Feature", del=False, key="archer-feature", choices=["","Sniper","Blitzer","Sentinel"] },
  powerChoiceField m "Encounter:" "archer-enc1" l1encounters]
  ++ (atLevel m 3 (powerChoiceField m "Encounter:" "archer-enc3" l3encounters))
  ++ (atLevel m 7 (powerChoiceField m "Encounter:" "archer-enc7" l7encounters))
  )]
