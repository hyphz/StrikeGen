module Classes.Bombardier exposing (classBombardier)

import ModelDB exposing (..)
import FormsModel exposing (..)
import PowerUtilities exposing (..)
import Dict


classBombardier : Class
classBombardier = { name = "Bombardier",
               classPowerList = powers,
               classForms = forms,
               classPowerBlocks = \m -> [],
               modifyBasicMelee = Just modifyBasicMelee,
               modifyBasicRange = Just modifyBasicRange,
               modifyRally = Nothing,
               modifyCharge = Nothing,
               modifyHP = Nothing,
               classFeats = [] }

atWillDamage : Model -> Int
atWillDamage m = if ((getLevel m) < 5) then 2 else 3

modifyBasicMelee : Model -> Power -> Power
modifyBasicMelee m p = {p | damage = atWillDamage m}


modifyBasicRange : Model -> Power -> Power
modifyBasicRange m p = {p | damage = atWillDamage m}

shapeSquare = levelTextSpecial "Shaped Blasts (Square)" [1,5]
shapeRing = levelTextSpecial "Shaped Blasts (Ring)" [1,5]
shapeCross = levelTextSpecial "Shaped Blasts (Cross)" [1,5]
shapeCone = levelTextSpecial "Shaped Blasts (Cone)" [1,5]
shapechoice m = Dict.fromList [("Square",shapeSquare m), ("Ring",shapeRing m),
                               ("Cross",shapeCross m), ("Cone",shapeCone m)]
misfire = levelTextSpecial "Misfire!" [1,5]

cshapes m = case (getResponse m "basics-role") of
  Just "Blaster" -> [misfire m, shapeSquare m, shapeRing m, shapeCross m, shapeCone m]
  _ -> [misfire m] ++ (powerlookup m "bombard-shape1" shapechoice) ++
                             (powerlookup m "bombard-shape2" shapechoice)

lastingBlasting = levelTextSpecial "Lasting Blasting" [1,9]
slowBurn m = levelTextPower "Delayed Fuse" Attack AtWill 10 0 (atWillDamage m) Green [1,9] m
bomberman m = levelTextPower "Bombing Run" Attack AtWill 10 0 (atWillDamage m) Green [1,9] m

specials m = Dict.fromList [("Lasting Blasting",lastingBlasting m),("Slow Burn",slowBurn m),
                          ("Bomberman",bomberman m)]
cspecial m = powerlookup m "bombard-feature" specials

l1atwills m = powerDict m [
    (quickPower "Kaboom" Misc AtWill 0 0 0 Green),
    (quickPower "Sonic Bomb" Misc AtWill 0 0 0 Green),
    (quickPower "Toxic Bomb" Misc AtWill 0 0 0 Green),
    (quickPower "Slime Bomb" Misc AtWill 0 0 0 Green),
    (quickPower "Hot Bomb" Misc AtWill 0 0 0 Green)]

l1awchosen m = [quickPower "Grenade" Attack AtWill 10 0 (atWillDamage m) Green m] ++
 powerlookup m "bombard-aw1" l1atwills ++ powerlookup m "bombard-aw2" l1atwills

l1encounters m = powerDict m [
   (quickPower "Energizing Bomb" Misc Encounter 0 0 0 Purple),
   (quickPower "Smoke Bomb" Misc Encounter 0 0 0 Purple),
   (quickPower "Sticky Bomb" Misc Encounter 0 0 0 Purple)]

l1echosen m = powerlookup m "bombard-enc1" l1encounters

l3encounters m = powerDict m [
  (quickPower "Healing Bomb" Misc Encounter 0 0 0 Purple),
  (quickPower "Icky Sticky Bomb" Misc Encounter 0 0 0 Purple),
  (quickPower "Weakening Bomb" Misc Encounter 0 0 0 Purple),
  (quickPower "Concussive Bomb" Misc Encounter 0 0 0 Purple),
  (quickPower "Getaway Bomb" Reaction Encounter 0 0 0 Purple)]

l3echosen m = powerlookup m "bombard-enc3" l3encounters

l7encounters m = powerDict m [
  (quickPower "Invigorating Bomb" Misc Encounter 0 0 0 Purple),
  (quickPower "Flattening Bomb" Misc Encounter 0 0 0 Purple),
  (quickPower "Flash Bomb" Misc Encounter 0 0 0 Purple),
  (quickPower "Big Kaboom" Misc Encounter 0 0 0 Purple)]

l7echosen m = powerlookup m "bombard-enc7" l7encounters

powers m = (cspecial m) ++ (cshapes m) ++ (l1awchosen m) ++ (l1echosen m)
           ++ (atLevelList m 3 (l3echosen m))
           ++ (atLevelList m 7 (l7echosen m))

forms m = [Form False "Bombardier" ([
  powerChoiceField m "Shape:" "bombard-shape1" shapechoice,
  powerChoiceField m "Shape:" "bombard-shape2" shapechoice,
  powerChoiceField m "Type:" "bombard-feature" specials,
  powerChoiceField m "At-Will:" "bombard-aw1" l1atwills,
  powerChoiceField m "At-Will:" "bombard-aw2" l1atwills,
  powerChoiceField m "Encounter:" "bombard-enc1" l1encounters]
  ++ (atLevel m 3 (powerChoiceField m "Encounter:" "bombard-enc3" l3encounters))
  ++ (atLevel m 7 (powerChoiceField m "Encounter:" "bombard-enc7" l7encounters))
  )]
