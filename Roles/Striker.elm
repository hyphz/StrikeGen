module Roles.Striker exposing (roleStriker)

import ModelDB exposing (..)
import FormsModel exposing (..)
import PowerUtilities exposing (..)


roleStriker : Role
roleStriker = { name = "Striker",
               rolePowerList = powers,
               roleForms = forms }


damageBoost m = if (getLevel m) < 4 then [quickSpecial "Damage Boost" m]
            else if (getLevel m) < 8 then [quickSpecial "Improved Damage Boost" m]
              else [quickSpecial "Super Damage Boost" m]

quickShift = levelTextPower "Quick Shift" RoleSlot Encounter 0 0 0 Blue [1,8]
drawABead = levelTextPower "Draw a Bead" RoleSlot AtWill 0 0 0 Blue [1,4,8]


actionTrigger m = if (getLevel m) < 6 then [quickPower "Strike Back" Reaction Encounter 0 0 0 Yellow m]
                                      else [quickPower "Dodge and Strike Back" Reaction Encounter 0 0 0 Yellow m]

otherBoost m = case (getResponse m "striker-boost") of
  Just "Mobility" -> [quickShift m]
  Just "Accuracy" -> [drawABead m]
  _ -> []


lightning = quickPower "Lightning Strikes" RoleSlot Encounter 0 0 0 Red
strikedodge = levelTextPower "Strike and Dodge" RoleSlot Encounter 0 0 0 Red [1,6,10]
windUpStrike m = let
  extraDamage = truncate (((toFloat (getLevel m)) / 2) + 3)
  in
    {
      name = "Wind Up Strike",
      slot = RoleSlot,
      freq = Encounter,
      range = 0,
      area = 0,
      damage = 0,
      styl = Red,
      text = "Deal " ++ (toString extraDamage) ++ " extra damage if you hit with your next attack. If you next attack hits multiple targets, apply this damage to only one of them."}
momentaryWeakness = quickPower "Momentary Weakness" RoleSlot Encounter 0 0 0 Red



upgraded x m = case x of
   "Lightning Strikes" -> [quickPower "Lightning Strikes Twice" RoleSlot Encounter 0 0 0 Red m]
   "Strike and Dodge" -> [quickPower "Strike and Run" RoleSlot Encounter 0 0 0 Red m]
   "Wind Up Strike" -> [quickPower "Power Up Strike" RoleSlot Encounter 0 0 0 Red m]
   "Momentary Weakness" -> [quickPower "Persistent Weakness" RoleSlot Encounter 0 0 0 Red m]
   _ -> []



checkUpgrade m p =
  if ((getLevel m) < 10) then p else
  case (List.head p) of
    Nothing -> p
    Just rp -> case (getResponse m "striker-upgrade") of
      Nothing -> p
      Just x -> if (x == rp.name) then upgraded rp.name m else p


encounters m = powerDict m [lightning, strikedodge, windUpStrike, momentaryWeakness]
l2encchosen m = checkUpgrade m (powerlookup m "striker-enc1" encounters)
l6encchosen m = checkUpgrade m (powerlookup m "striker-enc2" encounters)

upgradable m = [""] ++ (List.map .name (powerlookup m "striker-enc1" encounters  ++
                                        powerlookup m "striker-enc2" encounters))


powers m = damageBoost m ++ otherBoost m ++ actionTrigger m ++
  atLevelList m 2 (l2encchosen m) ++
  atLevelList m 6 (l6encchosen m)





forms m = [Form False "Striker" ([
  DropdownField { name="Boost:", del=False, key="striker-boost", choices=["","Mobility","Accuracy"] }]
  ++ atLevel m 2 (powerChoiceField m "Encounter:" "striker-enc1" encounters)
  ++ atLevel m 6 (powerChoiceField m "Encounter:" "striker-enc2" encounters)
  ++ atLevel m 10 (DropdownField { name="Upgrade:",del=False,key="striker-upgrade",choices=(upgradable m)})
  )]
