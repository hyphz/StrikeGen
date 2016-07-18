module Roles.Leader exposing (roleLeader)

import ModelDB exposing (..)
import FormsModel exposing (..)
import PowerUtilities exposing (..)
import String

roleLeader : Role
roleLeader = { name = "Leader",
               modifySpeed = Nothing,
               rolePowerList = (\m -> powers m ""),
               rolePowerListPrefix = powers,
               roleForms = (\m -> forms m ""),
               roleFormsPrefix = forms,
               roleFeats = ["Limber Leader"] }


actionTrigger m = if (getLevel m) < 6 then [quickPower "Try again!" Reaction Encounter 0 0 0 Yellow m]
                                      else [quickPower "Try harder!" Reaction Encounter 0 0 0 Yellow m]

boosts m = case (hasFeat m "Limber Leader") of
  False -> [levelTextPower "Tactics" RoleSlot Encounter 0 0 0 Red [1, 4, 8] m,
            levelTextPower "Heal" RoleSlot Encounter 5 0 0 Red [1, 4, 8] m]
  True -> [levelTextPower "Limber Tactics" RoleSlot Encounter 0 0 0 Red [1, 4, 8] m,
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



checkUpgrade m pr p =
  if ((getLevel m) < 10) then p else
  case (List.head p) of
    Nothing -> p
    Just rp -> case (getResponse m (pr ++ "leader-upgrade")) of
      Nothing -> p
      Just x -> if (x == rp.name) then upgraded rp.name m else p


l2encchosen m p = checkUpgrade m p (prefixpowerlookup m p "leader-enc1" encounters)
l6encchosen m p = checkUpgrade m p (prefixpowerlookup m p "leader-enc2" encounters)

upgradable m p = [""] ++ (List.map .name (prefixpowerlookup m p "leader-enc1" encounters  ++
                                        prefixpowerlookup m p "leader-enc2" encounters))

eggpower m = case (getResponse m "basics-name") of
  Nothing -> []
  Just x -> if ((String.toLower x) == "call me dave!") then
    [Power "We are not neutral" "Pick a random power that you do not have. All other players vote on whether you use it or not. If the vote is yes, admit you have no idea how you were going to use a power you don't have, and then leave the room." RoleSlot Encounter 0 0 0 Red]
  else []



powers m p = boosts m ++ actionTrigger m ++
  atLevelList m 2 (l2encchosen m p) ++
  atLevelList m 6 (l6encchosen m p) ++
  eggpower m






forms m p = [Form False "Leader" (
    atLevel m 2 (prefixpowerChoiceField m "Encounter:" p "leader-enc1" encounters)
  ++ atLevel m 6 (prefixpowerChoiceField m "Encounter:" p "leader-enc2" encounters)
  ++ atLevel m 10 (DropdownField { name="Upgrade:",del=False,key=(p++"leader-upgrade"),choices=(upgradable m p)})
  )]
