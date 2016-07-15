module Roles.Controller exposing (roleController, sapStrength)

import ModelDB exposing (..)
import FormsModel exposing (..)
import PowerUtilities exposing (..)


roleController : Role
roleController = { name = "Controller",
               rolePowerList = powers,
               roleForms = forms,
               modifySpeed = Nothing,
              roleFeats = ["Crafty Controller"] }


controlBoost m = if (getLevel m) < 4 then [quickSpecial "Control Boost" m]
            else if (getLevel m) < 8 then [quickSpecial "Improved Control Boost" m]
              else [quickSpecial "Super Control Boost" m]

sapStrength m = levelTextPower "Sap Strength" RoleSlot AtWill 5 0 0 Blue [1,4,8] m

actionTrigger m = if (getLevel m) < 6 then [quickPower "Freeze!" Reaction Encounter 0 0 0 Yellow m]
                                      else [quickPower "Slide!" Reaction Encounter 0 0 0 Yellow m]

boosts m = controlBoost m ++ [sapStrength m]




encounters m = powerDict m [
  quickPower "Save or Suck" RoleSlot Encounter 10 0 0 Red,
  quickPower "Stand Still" RoleSlot Encounter 10 0 0 Red,
  quickPower "Battlefield Repositioning" RoleSlot Encounter 0 5 0 Red,
  quickPower "Flash" RoleSlot Encounter 10 0 0 Red]


upgraded x m = case x of
  "Save or Suck" -> [quickPower "Save or Die" RoleSlot Encounter 10 0 0 Red m]
  "Stand Still" -> [quickPower "All Tied Up" RoleSlot Encounter 10 0 0 Red m]
  "Battlefield Repositioning" -> [quickPower "Warzone Repositioning" RoleSlot Encounter 0 10 0 Red m]
  "Flash" -> [quickPower "Solarbeam" RoleSlot Encounter 10 0 0 Red m]
  _ -> []



checkUpgrade m p =
  if ((getLevel m) < 10) then p else
  case (List.head p) of
    Nothing -> p
    Just rp -> case (getResponse m "controller-upgrade") of
      Nothing -> p
      Just x -> if (x == rp.name) then upgraded rp.name m else p


l2encchosen m = checkUpgrade m (powerlookup m "controller-enc1" encounters)
l6encchosen m = checkUpgrade m (powerlookup m "controller-enc2" encounters)

upgradable m = [""] ++ (List.map .name (powerlookup m "controller-enc1" encounters  ++
                                        powerlookup m "controller-enc2" encounters))


powers m = boosts m ++ actionTrigger m ++
  atLevelList m 2 (l2encchosen m) ++
  atLevelList m 6 (l6encchosen m) ++
    if (hasFeat m "Crafty Controller") then [quickSpecial "Crafty Controller" m] else []





forms m = [Form False "Controller" (
    atLevel m 2 (powerChoiceField m "Encounter:" "controller-enc1" encounters)
  ++ atLevel m 6 (powerChoiceField m "Encounter:" "controller-enc2" encounters)
  ++ atLevel m 10 (DropdownField { name="Upgrade:",del=False,key="controller-upgrade",choices=(upgradable m)})
  )]
