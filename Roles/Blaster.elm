module Roles.Blaster exposing (roleBlaster)

import ModelDB exposing (..)
import FormsModel exposing (..)
import PowerUtilities exposing (..)


roleBlaster : Role
roleBlaster = { name = "Blaster",
               rolePowerList = powers,
               roleForms = forms }


multiBoost m = if (getLevel m) < 4 then [quickSpecial "Multitarget Boost" m]
            else if (getLevel m) < 8 then [quickSpecial "Improved Multitarget Boost" m]
              else [quickSpecial "Super Multitarget Boost" m]

blasterBombardier m = case (getResponse m "basics-class") of
  Just "Bombardier" -> [quickSpecial "Blaster Bombardier" m]
  _ -> []

precision m = [levelTextPower "Precision" RoleSlot AtWill 0 0 0 Blue [1,4,8] m]
terrain m = [levelTextPower "Terrain" RoleSlot AtWill 0 0 0 Blue [1,4,8] m]

actionTrigger m = if (getLevel m) < 6 then [quickPower "Consistent Attack" Reaction Encounter 0 0 0 Yellow m]
                                      else [quickPower "Dependable" Reaction Encounter 0 0 0 Yellow m]

boosts m = multiBoost m ++ precision m ++ terrain m ++ blasterBombardier m




encounters m = powerDict m [
  quickPower "That Wasn't There Before" RoleSlot Encounter 0 0 0 Red,
  quickPower "Like Maple Syrup" RoleSlot Encounter 0 10 0 Red,
  quickPower "The Friend Zone" RoleSlot Encounter 0 0 0 Red,
  levelTextPower "The Not So Friendly Zone" RoleSlot Encounter 0 0 0 Red [1,6]]


upgraded x m = case x of
  "That Wasn't There Before" -> [quickPower "Where Did He Come From?" RoleSlot Encounter 0 0 0 Red m]
  "Like Maple Syrup" -> [quickPower "Like Concrete" RoleSlot Encounter 0 10 0 Red m]
  "The Friend Zone" -> [quickPower "The Friends with Benefits Zone" RoleSlot Encounter 0 0 0 Red m]
  "The Not So Friendly Zone" -> [quickPower "The Downright Unfriendly Zone" RoleSlot Encounter 0 0 0 Red m]
  _ -> []



checkUpgrade m p =
  if ((getLevel m) < 10) then p else
  case (List.head p) of
    Nothing -> p
    Just rp -> case (getResponse m "blaster-upgrade") of
      Nothing -> p
      Just x -> if (x == rp.name) then upgraded rp.name m else p


l2encchosen m = checkUpgrade m (powerlookup m "blaster-enc1" encounters)
l6encchosen m = checkUpgrade m (powerlookup m "blaster-enc2" encounters)

upgradable m = [""] ++ (List.map .name (powerlookup m "blaster-enc1" encounters  ++
                                        powerlookup m "blaster-enc2" encounters))


powers m = boosts m ++ actionTrigger m ++
  atLevelList m 2 (l2encchosen m) ++
  atLevelList m 6 (l6encchosen m)





forms m = [Form False "Blaster" (
    atLevel m 2 (powerChoiceField m "Encounter:" "blaster-enc1" encounters)
  ++ atLevel m 6 (powerChoiceField m "Encounter:" "blaster-enc2" encounters)
  ++ atLevel m 10 (DropdownField { name="Upgrade:",del=False,key="blaster-upgrade",choices=(upgradable m)})
  )]
