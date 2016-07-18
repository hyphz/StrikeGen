module Roles.Blaster exposing (roleBlaster, precision, terrain)

import ModelDB exposing (..)
import FormsModel exposing (..)
import PowerUtilities exposing (..)


roleBlaster : Role
roleBlaster = { name = "Blaster",
               rolePowerList = (\m -> powers m ""),
               rolePowerListPrefix = powers,
               roleForms = (\m -> forms m ""),
               roleFormsPrefix = forms,
               modifySpeed = Nothing,
               roleFeats = ["Beam Blaster","Boosted Blaster"] }



blastBoost m =  if (getLevel m) < 4 then [quickSpecial "Multitarget Blast Boost" m]
            else if (getLevel m) < 8 then [quickSpecial "Improved Multitarget Blast Boost" m]
              else [quickSpecial "Super Multitarget Blast Boost" m]

beamBoost m =  if (getLevel m) < 4 then [quickSpecial "Multitarget Beam Boost" m]
            else if (getLevel m) < 8 then [quickSpecial "Improved Multitarget Beam Boost" m]
              else [quickSpecial "Super Multitarget Beam Boost" m]

beamBlasterBoost m =  if (getLevel m) < 4 then [quickSpecial "Multitarget Beam Blaster" m]
            else if (getLevel m) < 8 then [quickSpecial "Improved Multitarget Beam Blaster" m]
              else [quickSpecial "Super Multitarget Beam Blaster" m]


multiBoost m p = if (hasFeat m "Beam Blaster") then beamBlasterBoost m else
  case (prefixgetResponse m p "blaster-type") of
    Just "Beams" -> beamBoost m
    _ -> blastBoost m



blasterBombardier m = case (getResponse m "basics-class") of
  Just "Bombardier" -> [quickSpecial "Blaster Bombardier" m]
  _ -> []



precision m = levelTextPower "Precision" RoleSlot AtWill 0 0 0 Blue [1,4,8] m
terrain m = levelTextPower "Terrain" RoleSlot AtWill 0 0 0 Blue [1,4,8] m

boostchoices m = powerDict m [precision, terrain]
cboost m p = if (hasFeat m "Boosted Blaster") then [precision m, terrain m] else prefixpowerlookup m p "blaster-boost" boostchoices

actionTrigger m = if (getLevel m) < 6 then [quickPower "Consistent Attack" Reaction Encounter 0 0 0 Yellow m]
                                      else [quickPower "Dependable" Reaction Encounter 0 0 0 Yellow m]

boosts m p = multiBoost m p ++ cboost m p ++ blasterBombardier m




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



checkUpgrade m pr p =
  if ((getLevel m) < 10) then p else
  case (List.head p) of
    Nothing -> p
    Just rp -> case (getResponse m (pr++"blaster-upgrade")) of
      Nothing -> p
      Just x -> if (x == rp.name) then upgraded rp.name m else p


l2encchosen m pr = checkUpgrade m pr (prefixpowerlookup m pr "blaster-enc1" encounters)
l6encchosen m pr = checkUpgrade m pr (prefixpowerlookup m pr "blaster-enc2" encounters)

upgradable m p = [""] ++ (List.map .name (prefixpowerlookup m p "blaster-enc1" encounters  ++
                                        prefixpowerlookup m p "blaster-enc2" encounters))


powers m p = boosts m p ++ actionTrigger m ++
  atLevelList m 2 (l2encchosen m p) ++
  atLevelList m 6 (l6encchosen m p)





forms m p = [Form False "Blaster" (
    [DropdownField { name="Type:", del=False, key=(p ++ "blaster-type"),choices=["","Blasts","Beams"] },
     prefixpowerChoiceField m "Boost:" p "blaster-boost" boostchoices] ++
    atLevel m 2 (prefixpowerChoiceField m "Encounter:" p "blaster-enc1" encounters)
  ++ atLevel m 6 (prefixpowerChoiceField m "Encounter:" p "blaster-enc2" encounters)
  ++ atLevel m 10 (DropdownField { name="Upgrade:",del=False,key=(p++"blaster-upgrade"),choices=(upgradable m p)})
  )]
