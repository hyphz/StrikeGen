module Classes.Buddies exposing (classBuddies)

import ModelDB exposing (..)
import FormsModel exposing (..)
import PowerUtilities exposing (..)
import String
import Dict


classBuddies : Class
classBuddies = { name = "Buddies",
               classPowerList = powers,
               classForms = forms,
               classPowerBlocks = \m -> [],
               modifyBasicMelee = Just modifyBasicMelee,
               modifyBasicRange = Just modifyBasicRange,
               modifyRally = Just (\m p -> {p | text = overtext m "BuddiesRally"} ),
               modifyCharge = Nothing }

atWillDamage m = if (getLevel m < 5) then 2 else 3

modifyBasicMelee : Model -> Power -> Power
modifyBasicMelee m p = {p | damage = atWillDamage m}

modifyBasicRange : Model -> Power -> Power
modifyBasicRange m p = {p | damage = atWillDamage m}

support = (levelTextSpecial "Support Tokens" [1,5,9])
rousing = (quickSpecial "Rousing")
enabling = (quickSpecial "Enabling")
incisive = (quickSpecial "Incisive")
specials m = powerDict m [rousing, enabling, incisive]
cspecial m = powerlookup m "warlord-special" specials

buddyActives = Dict.fromList [
  ("Pushy","N: Push 2. B: Push 2 and Prone until end of their turn. S: Throw 4 and Prone until end of their next turn, and any enemies adjacent to where the target lands are Prone until the end of their next turns."),
  ("Enervating","N: Slowed until end of turn. B: Weakened and Slowed until end of turn. S: Blinded until end of turn."),
  ("Focused","N: Marked until end of turn. B: Marked and if it grants an Opportunity while within reach it is knocked Prone (all until end of turn). S: Marked, knocked Prone, and standing up grants an Opportunity to your Buddy (all until end of turn)."),
  ("Scary","N: Harried until end of turn. B: Next time you hit the target it is Panicked on its next turn. S: Target is Panicked until the end of its next turn. Next turn, it is Harried until the end of its next turn."),
  ("Dizzying","N: Distracted until end of turn. B: Dazed until end of turn. S: Stunned until end of turn."),
  ("Handsy","N: Grabbed, but automatically succeeds at escape roll. B: Grabbed and Ongoing 2 Damage while Grabbed. S: Restrained until escape.")]

buddyPassives : Dict.Dict String (String, String, String)
buddyPassives = Dict.fromList [
  ("Tough",("","When your buddy takes at least 3 damage, it resists 1. ","Resist 1. ")),
  ("Fast",("Speed 6. ","Speed 10. ","Speed 14. ")),
  ("Long-Limbed",("","Reach 2. ","Reach 3. ")),
  ("Ranged",("","Range 5. ","Range 20. ")),
  ("Aware",("","+1 damage to Opportunities. ","+2 damage to Opportunities. ")),
  ("Brave",("","When you get hit and your Buddy is within 2, it can shift adjacent and take the hit for you. ","When you get hit and your Buddy is within 5, it can shift adjacent and take the hit for you. ")),
  ("Slippery",("","Ignore the first Opportunity for moving each turn. ","Do not grant Opportunities for moving. ")),
  ("Flying",("","Can spend a Move Action to gain Flying. Can stop Flying as part of any Move Action. ","Can spend a Move Action to gain Flying and fly up to its speed. Can stop Flying as part of any Move Action. ")),
  ("Big",("","Buddy is 2x2 and counts as Full Cover for allies. ","Buddy is 3x3 and counts as Full Cover for allies. "))
  ]


buddyActiveChoices m = mayList (getResponse m "buddy-a1") ++ mayList (getResponse m "buddy-a2")

buddyPassiveChoices m = mayList (getResponse m "buddy-p1") ++ mayList (getResponse m "buddy-p2") ++
                        atLevelList m 5 (mayList (getResponse m "buddy-p3")) ++
                        atLevelList m 9 ((mayList (getResponse m "buddy-p4")) ++ (mayList (getResponse m "buddy-p5")))

buddyHasPassive m p = List.member p (buddyPassiveChoices m)

buddyHasBoostedPassive m p = (List.length (List.filter (\x -> x == p) (buddyPassiveChoices m))) > 1

buddyHasActive m p = List.member p (buddyActiveChoices m)

buddyPassiveDescriptor m k =
  let
    (nothave, have, boosted) = case (Dict.get k buddyPassives) of
      Just x -> x
      Nothing -> ("Passive is broken","Passive is broken","Passive is broken")
  in
    if (not (buddyHasPassive m k)) then nothave else
      if (buddyHasBoostedPassive m k) then boosted else have

buddyActiveDescriptor m k =
  let
    activeDesc = case (Dict.get k buddyActives) of
      Just x -> x
      Nothing -> "Active is broken"
    in
      if (buddyHasActive m k) then "**" ++ k ++ ":** " ++ activeDesc ++ "\n\n" else ""


buddyHitPoints m =
  case (getResponse m "buddy-type") of
    Just "Super Buddy" -> "8 HP. "
    _ -> "5 HP. "

buddyText m = overtext m "BuddyBasics" ++ "\n\n" ++ buddyHitPoints m ++ String.concat (List.map (buddyPassiveDescriptor m) (Dict.keys buddyPassives)) ++
  "\n\nActive Effects:\n\n" ++ String.concat (List.map (buddyActiveDescriptor m) (Dict.keys buddyActives))

buddySpecial m = Power "Buddy" (buddyText m) Misc None 0 0 0 White

basePowers m = [quickPower "Team Attack" Attack AtWill -10 0 (atWillDamage m) Green m,
                quickPower "Recall Buddy" Attack AtWill 0 0 0 Green m,
                quickPower "Boosted Buddy" Attack Encounter 0 0 3 Purple m]

l1atwills m = powerDict m [quickPower "Coordinated Attack" Attack AtWill 0 0 0 Green,
               quickPower "We Are One" Attack AtWill 0 0 0 Green,
               quickPower "Command Buddy" Attack AtWill 0 0 2 Green]
l1chosen m = powerlookup m "buddy-aw1" l1atwills ++ powerlookup m "buddy-aw2" l1atwills

l3encounters m = powerDict m [quickPower "Wrestling Combo" Attack Encounter 0 0 3 Purple,
                  quickPower "Trip and Stab" Attack Encounter 0 0 0 Purple,
                  quickPower "Hold Still" Attack Encounter 0 0 0 Purple,
                  quickPower "Fight Back to Back" Attack Encounter 0 0 3 Purple]
l3chosen m = powerlookup m "buddy-enc" l3encounters


buddyActiveChoice m k =
  DropdownField {name="Active:", del=False, key=k, choices=[""] ++ (Dict.keys buddyActives)}

type PassiveChoiceType = MustNew | MustBoost | Either

buddyPassiveChoice m k x =
  DropdownField {del=False, key=k, choices=case x of
    Either -> [""] ++ (Dict.keys buddyPassives)
    MustNew -> [""] ++ List.sort (List.filter (\x -> not (List.member x (buddyPassiveChoices m))) (Dict.keys buddyPassives)
                    ++ mayList (getResponse m k))
    MustBoost -> [""] ++ List.sort (List.filter (\x -> (List.member x (buddyPassiveChoices m))) (Dict.keys buddyPassives)),
  name = case x of
    Either -> "Passive/Boost:"
    MustNew -> "Passive:"
    MustBoost -> "Boost:"
  }


defenderBuddyOption m =
  if (getResponse m "basics-role" /= Just "Defender") then [] else
    if (getResponse m "buddy-type" /= Just "Super Buddy") then [] else
      [DropdownField {name="Defender:", del=False, key="buddy-def", choices=["","You","Buddy"]}]

defenderBuddyPower m =
  if (getResponse m "basics-role" /= Just "Defender") then [] else
    if (getResponse m "buddy-type" /= Just "Super Buddy") then [] else
      if (getResponse m "buddy-def" /= Just "Buddy") then [] else [quickSpecial "Defender Buddy" m]


powers m = basePowers m ++ [buddySpecial m] ++ defenderBuddyPower m ++ l1chosen m ++
   atLevelList m 3 (l3chosen m) ++
   atLevel m 7 (quickPower "Super Buddy" Attack Encounter 0 0 4 Purple m)

forms m = [Form False "Buddies" ([
  DropdownField {name="Type:", del=False, key="buddy-type", choices=["","Regular Buddy","Super Buddy"]},
  powerChoiceField m "At-Will:" "buddy-aw1" l1atwills,
  powerChoiceField m "At-Will:" "buddy-aw2" l1atwills,
  buddyActiveChoice m "buddy-a1",
  buddyActiveChoice m "buddy-a2",
  buddyPassiveChoice m "buddy-p1" MustNew,
  buddyPassiveChoice m "buddy-p2" MustNew]
  ++ defenderBuddyOption m
  ++ atLevel m 3 (powerChoiceField m "Encounter:" "buddy-enc" l3encounters)
  ++ atLevel m 5 (buddyPassiveChoice m "buddy-p3" Either)
  ++ atLevelList m 9 [buddyPassiveChoice m "buddy-p4" MustBoost, buddyPassiveChoice m "buddy-p5" MustNew])]


--  powerChoiceField m "Feature:" "warlord-special" specials,
--  powerChoiceField m "At-Will:" "warlord-aw1" l1atwills,
--  powerChoiceField m "At-Will:" "warlord-aw2" l1atwills,
--  powerChoiceField m "Encounter:" "warlord-enc1" l1encounters]
--  ++ (atLevel m 3 (powerChoiceField m "Encounter:" "warlord-enc3" l3encounters))
--  ++ (atLevel m 7 (powerChoiceField m "Encounter:" "warlord-enc7" l7encounters))
