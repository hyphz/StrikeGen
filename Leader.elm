module Leader exposing (roleLeader)

import ModelDB exposing (..)
import FormsModel exposing (..)
import Dict exposing (..)
import PowerUtilities exposing (..)


roleLeader : Role
roleLeader = { name = "Leader",
               rolePowerList = powers,
               roleForms = forms }


actionTrigger m = if (getLevel m) < 6 then [quickPower "Try again!" Reaction Encounter 0 0 0 Yellow m]
                                      else [quickPower "Try harder!" Reaction Encounter 0 0 0 Yellow m]

boosts m = [levelTextPower "Tactics" RoleSlot Encounter 0 0 0 Red [1, 4, 8] m,
            levelTextPower "Heal" RoleSlot Encounter 5 0 0 Red [1, 4, 8] m]




encounters m = powerDict m [
  quickPower "Hit Him!" RoleSlot Encounter 0 0 0 Red,
  quickPower "Walk it off!" RoleSlot Encounter 0 0 0 Red,
  quickPower "Mass Heal" RoleSlot Encounter 0 0 0 Red,
  quickPower "Keep Walking!" Reaction Encounter 0 0 0 Red,
  quickPower "Reveal Weakness" RoleSlot Encounter 0 0 0 Red]



upgraded x m = case x of
  "Hit Him!" -> [quickPower "Hit Him Harder!" RoleSlot Encounter 0 0 0 Red m]
  "Walk it off!" -> [quickPower "Walk it off harder!" RoleSlot Encounter 0 0 0 Red m]
  "Mass Heal" -> [quickPower "Fresh Start" RoleSlot Encounter 0 0 0 Red m]
  "Keep Walking!" -> [quickPower "You're Fine!" Reaction Encounter 0 0 0 Red m]
  "Reveal Weakness" -> [quickPower "Reveal Weak Point" Reaction Encounter 0 0 0 Red m]
  _ -> []



checkUpgrade m p =
  if ((getLevel m) < 10) then p else
  case (List.head p) of
    Nothing -> p
    Just rp -> case (getResponse m "leader-upgrade") of
      Nothing -> p
      Just x -> if (x == rp.name) then upgraded rp.name m else p


l2encchosen m = checkUpgrade m (powerlookup m "leader-enc1" encounters)
l6encchosen m = checkUpgrade m (powerlookup m "leader-enc2" encounters)

upgradable m = [""] ++ (List.map .name (powerlookup m "leader-enc1" encounters  ++
                                        powerlookup m "leader-enc2" encounters))


powers m = boosts m ++ actionTrigger m ++
  atLevelList m 2 (l2encchosen m) ++
  atLevelList m 6 (l6encchosen m)





forms m = [Form False "Leader" (
    atLevel m 2 (powerChoiceField m "Encounter:" "leader-enc1" encounters)
  ++ atLevel m 6 (powerChoiceField m "Encounter:" "leader-enc2" encounters)
  ++ atLevel m 10 (DropdownField { name="Upgrade:",del=False,key="leader-upgrade",choices=(upgradable m)})
  )]
