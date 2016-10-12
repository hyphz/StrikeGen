module TacticalModel exposing (..)
import Classes.Necromancer exposing (classNecro)
import Classes.Archer exposing (classArcher)
import Classes.Duelist exposing (classDuelist)
import Classes.MartialArtist exposing (classMA)
import Classes.Simplified exposing (classSimplified)
import Classes.Warlord exposing (classWarlord)
import Classes.Magician exposing (classMagician)
import Classes.Bombardier exposing (classBombardier)
import Classes.Summoner exposing (classSummoner)
import Classes.Shapechanger exposing (classShapechanger)
import Classes.Buddies exposing (classBuddies)
import Roles.Striker exposing (roleStriker)
import Roles.Leader exposing (roleLeader)
import Roles.Defender exposing (roleDefender)
import Roles.Controller exposing (roleController)
import Roles.Blaster exposing (roleBlaster)
import ModelDB exposing (..)
import FormsModel exposing (..)
import Dict exposing (Dict)
import PowerUtilities exposing (..)

{-| Returns the list of available implemented classes. -}
classes : Dict String Class
classes = Dict.fromList [("Archer",classArcher),
                         ("Bombardier",classBombardier),
                         ("Buddies",classBuddies),
                         ("Duelist",classDuelist),
                         ("Martial Artist",classMA),
                         ("Magician",classMagician),
                         ("Necromancer",classNecro),
                         ("Shapechanger",classShapechanger),
                         ("Simplified",classSimplified),
                         ("Summoner",classSummoner),
                         ("Warlord",classWarlord)
                         ]

{-| Returns the list of available implemented roles. -}
roles : Dict String Role
roles = Dict.fromList [("Blaster",roleBlaster),("Controller",roleController),
                     ("Defender",roleDefender),("Leader",roleLeader),("Striker",roleStriker)]


{-| Accepts a key, a dict, a function, and a value. Looks up the dict in the given list. If it's
found, runs the function or it, expecting to get a Maybe-Function. If it gets one, run that on the
given value and return the result. If anything's not found or the returned value is Nothing,
return the value untouched. Used for looking up functions inside the selected class that modify
standard values. -}
applyGenericModifier : Model -> String -> Dict String a -> (a -> Maybe (Model -> b -> b)) -> b -> b
applyGenericModifier m key list extractor value =
  let c = indirectLookup m key list (\x -> (extractor x)) Nothing Nothing in
  case c of
    Nothing -> value
    Just func -> func m value


{-| Looks up the current class and applies a function (probably a field extractor) to it.
That function is expected to return another function or Nothing. If it returns a function,
apply it to the given value. -}
applyClassModifier : Model -> (Class -> Maybe (Model -> a -> a)) -> a -> a
applyClassModifier m extractor value = applyGenericModifier m "basics-class" classes extractor value

{-| Same as applyClassModifier, but for roles. -}
applyRoleModifier : Model -> (Role -> Maybe (Model -> a -> a)) -> a -> a
applyRoleModifier m extractor value = applyGenericModifier m "basics-role" roles extractor value

{-| The melee basic attack. -}
pmeleeBasic : Model -> Power
pmeleeBasic m = applyClassModifier m .modifyBasicMelee {name = "Melee Basic Attack",
               text = "No effect.",
               slot = Attack,
               freq = AtWill,
               range = 0,
               area = 0,
               damage = 2,
               styl = Green
               }

{-| The ranged basic attack. -}
prangedBasic : Model -> Power
prangedBasic m = applyClassModifier m .modifyBasicRange {name = "Ranged Basic Attack",
               text = "No effect.",
               slot = Attack,
               freq = AtWill,
               range = 5,
               area = 0,
               damage = 2,
               styl = Green
               }

{-| The charge power. -}
pcharge : Model -> Power
pcharge m = applyClassModifier m .modifyCharge {name = "Charge",
               text = overtext m "GlobalCharge",
               slot = Attack,
               freq = AtWill,
               range = 0,
               area = 0,
               damage = 0,
               styl = Green
               }

{-| The Rally power. -}
pRally : Model -> Power
pRally m = applyClassModifier m .modifyRally {name = "Rally",
               text = overtext m "GlobalRally",
               slot = Misc,
               freq = Encounter,
               range = 0,
               area = 0,
               damage = 0,
               styl = Yellow
               }

{-| The assess power. -}
pAssess : Model -> Power
pAssess m = {name = "Assess",
               text = overtext m "GlobalAssess",
               slot = RoleSlot,
               freq = AtWill,
               range = 0,
               area = 0,
               damage = 0,
               styl = Blue
               }

nullPowerModifier : Model -> Power -> Power
nullPowerModifier _ p = p

{-| List of the basic powers all characters have. -}
basicPowers : Model -> List Power
basicPowers m = [pmeleeBasic m, prangedBasic m, pcharge m, pRally m, pAssess m]

{-| Looks up and calls the function to get the selected class' power list. Note that it will
vary based on decisions made in the class form. -}
classPowers : Model -> List Power
classPowers m = indirectLookup m "basics-class" classes (\x -> x.classPowerList m) [] []

{-| Ditto but looks up the class' power block list. -}
classPowerBlocks : Model -> List PowerBlock
classPowerBlocks m = indirectLookup m "basics-class" classes (\x -> x.classPowerBlocks m) [] []

{-| Gets all class powers, whether they're in blocks or not. -}
allClassPowers : Model -> List Power
allClassPowers m = classPowers m ++ List.concatMap .powers (classPowerBlocks m)

{-| Gets all role powers for the selected role. If an MRS, no role powers are returned because
they are placed in the power blocks for the Shapechanger class and other parts of the system
consider the PC not to have them. -}
rolePowers : Model -> List Power
rolePowers m = if (isMultiRoleShaper m) then []
  else indirectLookup m "basics-role" roles (\x -> x.rolePowerList m) [] []

{-| Gets all forms. This is the class form plus the role form. For an MRS, no role form is
added because they are added by the Shapechanger class. -}
tacticalForms : Model -> List Form
tacticalForms m = (indirectLookup m "basics-class" classes (\x -> x.classForms m) [] []) ++
      if (isMultiRoleShaper m) then [] else (indirectLookup m "basics-role" roles (\x -> x.roleForms m) [] [])

{-| Get all powers to be shown on the sheet outside blocks. -}
getPowers : Model -> List Power
getPowers m = basicPowers m ++ classPowers m ++ rolePowers m ++ featPowers m

{-| Get all power blocks to be shown on the sheet. -}
getPowerBlocks : Model -> List PowerBlock
getPowerBlocks m = classPowerBlocks m

{-| Get all powers of any kind. Watch out for recursion if powers depend on each other. -}
getAllPowers : Model -> List Power
getAllPowers m = basicPowers m ++ allClassPowers m ++ rolePowers m ++ featPowers m

{-| The list of feats that give powers, and the powers they give. -}
powerFeats : Model -> Dict String (List Power)
powerFeats m = Dict.fromList ([
  ("Lucky",[quickPower "Lucky Dodger" Misc Encounter 0 0 0 Yellow m, quickPower "Lucky Escaper" Misc Encounter 0 0 0 Yellow m]),
  ("Melee Shooter",[quickSpecial "Melee Shooter" m]),
  ("Slippery",[quickSpecial "Slippery" m]),
  ("Like a Bull",[quickSpecial "Like a Bull" m]),
  ("Fast Reactions",[quickSpecial "Fast Reactions" m]),
  ("Resilient",[quickSpecial "Resilient" m]),
  ("Nothing Fazes You",[quickSpecial "Nothing Fazes You" m]),
  ("Flyer",[quickSpecial "Flyer" m]),
  ("Wrestler",[quickSpecial "Wrestler" m]),
  ("Long Reach",[quickSpecial "Long Reach" m, quickPower "Long Reach Knockdown" Misc Encounter 0 0 0 Yellow m]),
  ("Cleave",[quickSpecial "Cleave" m]),
  ("Stealthy",[quickSpecial "Stealthy" m, quickPower "Hide" Move AtWill 0 0 0 Yellow m]),
  ("Sprinter",[quickSpecial "Sprinter" m]),
  ("Toughness",[quickSpecial "Toughness" m]),
  ("Huge",[quickSpecial "Huge" m]),
  ("Steady Hands",[quickSpecial "Steady Hands" m]),
  ("Gang",[quickSpecial "Gang" m]),
  ("Spellbreak",[quickSpecial "Spellbreak" m]),
  ("Combat Resist",[quickSpecial "Combat Resist" m])
  ] ++
  (if (getResponse m "basics-class" /= Just "Summoner") then [("Reliable",[quickSpecial "Reliable" m])] else []) ++
  (if (hasFeat m "Combat Resist") then [("Area Resist",[quickSpecial "Area Immune" m]),("Combat Immune",[quickSpecial "Combat Immune" m])] else []))

{-| Gets the character's Hit Points by modifying 10. -}
getHP : Model -> Int
getHP m = applyClassModifier m .modifyHP 10 + (if hasFeat m "Toughness" then 3 else 0) + (if hasFeat m "Huge" then 3 else 0)

{-| Gets the character's Speed by modifying 6. -}
getSpeed : Model -> Int
getSpeed m = applyRoleModifier m .modifySpeed 6 +
  (if hasFeat m "Sprinter" then 4 else 0)

{-| Does the character have the named class? -}
hasClass : Model -> String -> Bool
hasClass m f = getResponse m "basics-class" == Just f

{-| Does the character not have the named class? -}
notClass : Model -> String -> Bool
notClass m f = not (hasClass m f)

{-| Does the character have the named role? MRSs have no role for this purpose. -}
hasRole : Model -> String -> Bool
hasRole m f = getResponse m "basics-role" == Just f

{-| Does the character not have the named role? MRSs have no role for this purpose. -}
notRole : Model -> String -> Bool
notRole m f = not (hasRole m f)

{-| Gets from the selected class the list of available feat names specific to that class. -}
availableClassFeats : Model -> List String
availableClassFeats m = indirectLookup m "basics-class" classes .classFeats [] []

{-| Multi Role Shapeshifters (MRS) are a pain in the ass and break a whole bunch of rules.
Here's a function to determine if we need to invoke their stupid number of special cases. -}
isMultiRoleShaper : Model -> Bool
isMultiRoleShaper m = (hasClass m "Shapechanger") && (hasFeat m "Multi-Role Shapechanger")

{-| Gets a list of all avilable feats from our role. MRSs can take feats from any of their
roles. -}
availableRoleFeats : Model -> List String
availableRoleFeats m =
  if (isMultiRoleShaper m) then
      case (getResponse m "shaper-type") of
          Just "Multi-Form" -> indirectLookup m "shaper-shape1-role" roles .roleFeats [] []
                              ++ indirectLookup m "shaper-shape2-role" roles .roleFeats [] []
                              ++ indirectLookup m "shaper-shape3-role" roles .roleFeats [] []
          Just "One-Form" -> indirectLookup m "shaper-shape1-role" roles .roleFeats [] []
          _ -> []
  else
      indirectLookup m "basics-role" roles .roleFeats [] []

{-| Get a list of available feets from class, role, and standards. Note that MRSs can take
Minor Role feats for any role to use when they're in a form that doesn't have that role. -}
availableFeats : Model -> List String
availableFeats m = "" :: List.sort (Dict.keys (powerFeats m) ++
  ["Superhuman", "Versatile Minor"] ++
  (if (notClass m "Shapechanger") then ["Bread and Butter"] else []) ++
  (if (notRole m "Striker") then ["Minor Striker"] else []) ++
  (if (notRole m "Leader") then ["Minor Leader"] else []) ++
  (if (notRole m "Defender") then ["Minor Defender"] else []) ++
  (if (notRole m "Blaster") then ["Minor Blaster"] else []) ++
  (if (notRole m "Controller") then ["Minor Controller"] else []) ++
  availableClassFeats m ++
  availableRoleFeats m)

{-| The special choice field for the Bread and Butter feat. -}
breadButterChoice : Model -> Field
breadButterChoice m = DropdownField {name="Bread and Butter:", del=False,
     key="bbfeat-atwill",
     choices="" :: (List.map .name <| List.filter (\x -> (x.slot == Attack && x.freq == AtWill)) ((allClassPowers m) ++ (rolePowers m)))}

{-| The special power for the Bread and Butter feat. -}
breadButterPower : Model -> List Power
breadButterPower m = case (getResponse m "bbfeat-atwill") of
  Nothing -> []
  Just bbpname -> if (not (List.member bbpname (List.map .name ((allClassPowers m) ++ (rolePowers m))))) then [] else
           [Power "Bread and Butter" ("Once per turn, when any power or ability allows you to make a Basic Attack, you may use " ++ bbpname ++ " instead.") Misc None 0 0 0 White]

{-| The list of available movement powers given by the Superhuman feat. -}
superhumanMovementPowers : Model -> Dict String Power
superhumanMovementPowers m = powerDict m
  [quickSpecial "Super Climbing",
  quickSpecial "Super Swimming",
  quickSpecial "Super Leaping",
  quickSpecial "Super Swapping",
  quickSpecial "Super Phasing"]

{-| The special form fields added for the Superhuman feat. -}
superhumanChoices : Model -> List Field
superhumanChoices m =
  [FreeformField {name="Super Sense:",del=False,key="super-sense"},
  powerChoiceField m "Super Movement:" "super-move" superhumanMovementPowers]

{-| List of all powers granted by the Superhuman feat. -}
superhumanPowers : Model -> List Power
superhumanPowers m =
  powerlookup m "super-move" superhumanMovementPowers ++
  case (getResponse m "super-sense") of
    Nothing -> []
    Just s -> [Power s ("You can ignore Blinded in circumstances where this power would make it appropriate.") Misc None 0 0 0 White]

{-| Modifies a Power to be Encounter and to be yellow typeset. Used for the Versatile Minor
feat to turn the at-will powers of classes into feat-granted encounter powers. -}
powerToFeatEncounter : Power -> Power
powerToFeatEncounter p = {p | freq = Encounter, styl = Yellow}

{-| Get all powers granted by Versatile Minor. MRS gets all of them. -}
versatileMinorPowers : Model -> List Power
versatileMinorPowers m =
  (if (notRole m "Blaster") then [powerToFeatEncounter (Roles.Blaster.precision m), powerToFeatEncounter (Roles.Blaster.terrain m)] else []) ++
  (if (notRole m "Controller") then [powerToFeatEncounter (Roles.Controller.sapStrength m)] else []) ++
  (if (notRole m "Defender") then [powerToFeatEncounter (Roles.Defender.mark m)] else []) ++
  (if (notRole m "Striker") then [powerToFeatEncounter (Roles.Striker.quickShift m), powerToFeatEncounter (Roles.Striker.drawABead m)] else []) ++
  [quickPower "Versatile Leader" Misc Encounter 0 0 0 Yellow m]

{-| Get all powers granted by Minor Role feats. -}
minorFeatPowers : Model -> List Power
minorFeatPowers m =
  (if (hasFeat m "Minor Striker" && notRole m "Striker") then [levelTextPower "Minor Striker" Misc Encounter 0 0 0 Yellow [1,4,8] m, powerToFeatEncounter <| Roles.Striker.quickShift m] else []) ++
  (if (hasFeat m "Minor Leader" && notRole m "Leader") then [quickPower "Minor Leader Slide" Misc Encounter 0 0 0 Yellow m, levelTextPower "Minor Leader Heal" RoleSlot Encounter 0 0 0 Yellow [1,4,8] m] else []) ++
  (if (hasFeat m "Minor Defender" && notRole m "Defender") then [quickSpecial "Minor Defender" m, powerToFeatEncounter <| Roles.Defender.mark m] else []) ++
  (if (hasFeat m "Minor Blaster" && notRole m "Blaster") then [quickPower "Minor Blaster" Misc Encounter 0 0 0 Yellow m, powerToFeatEncounter <| Roles.Blaster.terrain m] else []) ++
  (if (hasFeat m "Minor Controller" && notRole m "Controller") then [quickPower "Minor Controller" Misc Encounter 0 0 0 Yellow m, powerToFeatEncounter <| Roles.Controller.sapStrength m] else []) ++
  (if (hasFeat m "Versatile Minor") then versatileMinorPowers m else [])

{-| Gets all extra form choices triggered by feats. -}
featChoices : Model -> List Field
featChoices m = (if (hasFeat m "Bread and Butter" && notClass m "Shapechanger") then [breadButterChoice m] else []) ++
                (if (hasFeat m "Superhuman") then superhumanChoices m else [])

{-| Gets all powers granted by feats. -}
featPowers : Model -> List Power
featPowers m =
  let
    selectedPowerFeats = List.filter (\x -> List.member x (Dict.keys <| powerFeats m)) (chosenFeats m)
    powersForThoseFeats = List.concatMap (\x -> List.concat <| mayList (Dict.get x (powerFeats m))) selectedPowerFeats
  in
    powersForThoseFeats ++ minorFeatPowers m ++
      if (hasFeat m "Bread and Butter" && notClass m "Shapechanger") then breadButterPower m else [] ++
      if (hasFeat m "Superhuman") then superhumanPowers m else []
