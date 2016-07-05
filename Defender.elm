module Defender exposing (roleDefender)

import ModelDB exposing (..)
import FormsModel exposing (..)
import Dict exposing (..)
import PowerUtilities exposing (..)


roleDefender : Role
roleDefender = { name = "Defender",
               rolePowerList = powers,
               roleForms = forms }


defenseBoost m = if (getLevel m) < 4 then [quickSpecial "Defense Boost" m]
            else if (getLevel m) < 8 then [quickSpecial "Improved Defense Boost" m]
              else [quickSpecial "Super Defense Boost" m]
stickyBoost m = if (getLevel m) < 4 then [quickSpecial "Stickiness Boost" m]
            else if (getLevel m) < 8 then [quickSpecial "Improved Stickiness Boost" m]
              else [quickSpecial "Super Stickiness Boost" m]



actionTrigger m = if (getLevel m) < 6 then [quickPower "Call that a punch?" Reaction Encounter 0 0 0 Yellow m]
                                      else [quickPower "THIS is a punch!" Reaction Encounter 0 0 0 Yellow m]

boosts m = defenseBoost m ++ stickyBoost m ++
           [levelTextPower "Mark" RoleSlot AtWill 5 0 0 Blue [1, 4, 8] m]




encounters m = powerDict m [
  quickPower "I don't think so!" Reaction Encounter 0 0 0 Red,
  quickPower "Come and Get It!" RoleSlot Encounter 0 2 0 Red,
  quickPower "You're Mine!" RoleSlot Encounter 0 0 0 Red,
  quickPower "I'll Cover You!" Reaction Encounter 0 0 0 Red]


upgraded x m = case x of
  "I don't think so!" -> [quickPower "I really don't think so!" Reaction Encounter 0 0 0 Red m]
  "Come and Get It!" -> [quickPower "Come and Get Seconds!" RoleSlot Encounter 0 3 0 Red m]
  "You're Mine!" -> [quickPower "You're All Mine!" RoleSlot Encounter 0 0 0 Red m]
  "I'll Cover You!" -> [quickPower "I'll Cover You All!" Reaction Encounter 0 0 0 Red m]
  _ -> []



checkUpgrade m p =
  if ((getLevel m) < 10) then p else
  case (List.head p) of
    Nothing -> p
    Just rp -> case (getResponse m "defender-upgrade") of
      Nothing -> p
      Just x -> if (x == rp.name) then upgraded rp.name m else p


l2encchosen m = checkUpgrade m (powerlookup m "defender-enc1" encounters)
l6encchosen m = checkUpgrade m (powerlookup m "defender-enc2" encounters)

upgradable m = [""] ++ (List.map .name (powerlookup m "defender-enc1" encounters  ++
                                        powerlookup m "defender-enc2" encounters))


powers m = boosts m ++ actionTrigger m ++
  atLevelList m 2 (l2encchosen m) ++
  atLevelList m 6 (l6encchosen m)





forms m = [Form False "Leader" (
    atLevel m 2 (powerChoiceField m "Encounter:" "defender-enc1" encounters)
  ++ atLevel m 6 (powerChoiceField m "Encounter:" "defender-enc2" encounters)
  ++ atLevel m 10 (DropdownField { name="Upgrade:",del=False,key="defender-upgrade",choices=(upgradable m)})
  )]
