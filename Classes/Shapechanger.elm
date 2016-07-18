module Classes.Shapechanger exposing (classShapechanger)

import ModelDB exposing (..)
import FormsModel exposing (..)
import PowerUtilities exposing (..)
import Dict
import List
import Roles.Striker exposing (roleStriker)
import Roles.Leader exposing (roleLeader)
import Roles.Defender exposing (roleDefender)
import Roles.Controller exposing (roleController)
import Roles.Blaster exposing (roleBlaster)


type alias Shape = {
   name : String,
   transform : (Model -> Power),
   special : (Model -> Power),
   atwill1 : (Model -> Power),
   atwill2 : (Model -> Power),
   l3enc : (Model->Power),
   l7enc : (Model->Power)
}

atWillDamage : Model -> Int
atWillDamage m = if ((getLevel m) < 5) then 2 else 3

tortoise = Shape "Tortoise"
  (quickPower "Tortoise's Transformation" Attack Encounter 0 0 3 Purple)
  (levelTextSpecial "Form of the Tortoise" [1, 5, 9])
  (quickPower "Total Defense" Attack AtWill 0 0 0 Green)
  (\m -> quickPower "Shield" Attack AtWill 0 0 (atWillDamage m) Green m)
  (quickPower "Perfect Fortress" Attack Encounter 0 0 3 Purple)
  (quickPower "Shield For All" Attack Encounter 0 0 4 Purple)

bull = Shape "Bull"
  (quickPower "Bull's Transformation" Attack Encounter 0 0 3 Purple)
  (levelTextSpecial "Form of the Bull" [1, 5, 9])
  (\m -> quickPower "Bull Rush" Attack AtWill 0 0 (atWillDamage m) Green m)
  (quickPower "Nimble Charge" Attack AtWill 0 0 0 Green)
  (quickPower "Shove and Follow" Attack Encounter 0 0 0 Purple)
  (quickPower "Crash Test" Attack Encounter 0 0 4 Purple)

hawk = Shape "Hawk"
  (quickPower "Hawk's Transformation" Attack Encounter 0 0 3 Purple)
  (levelTextSpecial "Form of the Hawk" [1, 5, 9])
  (\m -> quickPower "Fly" Attack AtWill 0 0 (atWillDamage m) Green m)
  (\m -> quickPower "Diving Feint" Attack AtWill 0 0 (atWillDamage m) Green m)
  (quickPower "Dust Bath" Attack Encounter 0 0 3 Purple)
  (quickPower "Live Bomb" Attack Encounter 0 0 4 Purple)

mammoth = Shape "Mammoth"
  (levelTextPower "Mammoth's Transformation" Attack Encounter 0 0 3 Purple [1,9])
  (levelTextSpecial "Form of the Mammoth" [1, 5])
  (\m -> levelTextPower "Pushy" Attack AtWill 0 0 (atWillDamage m) Green [1,5,9] m)
  (\m -> levelTextPower "Stompy" Attack AtWill 0 0 (atWillDamage m) Green [1,5,9] m)
  (quickPower "Fastball Special" Attack Encounter 0 0 0 Purple)
  (quickPower "Fastball Deluxe" Attack Encounter 0 0 4 Purple)

viper = Shape "Viper"
  (quickPower "Viper's Transformation" Attack Encounter 0 0 3 Purple)
  (levelTextSpecial "Form of the Viper" [1, 5, 9])
  (\m -> levelTextPower "Venom" Attack AtWill 0 0 (atWillDamage m) Green [1,5] m)
  (\m -> quickPower "Enervation" Attack AtWill 0 0 (atWillDamage m) Green m)
  (quickPower "Spit Poison" Attack Encounter 5 0 3 Purple)
  (quickPower "Inescapable Venom" Attack Encounter 0 0 4 Purple)

kraken = Shape "Kraken"
  (quickPower "Kraken's Transformation" Attack Encounter 0 0 3 Purple)
  (levelTextSpecial "Form of the Kraken" [1, 5, 9])
  (\m -> quickPower "Grab" Attack AtWill 0 0 (atWillDamage m) Green m)
  (quickPower "Crush" Attack AtWill 0 0 0 Green)
  (quickPower "Choke Out" Attack Encounter 5 0 3 Purple)
  (quickPower "Death Grip" Attack Encounter 0 0 4 Purple)

shapes =
  let
    shapepair s = (s.name, s)
  in
    Dict.fromList (List.map shapepair [tortoise, bull, hawk, mammoth, viper, kraken])

shapeBbpower m k shape =
  if (lacksFeat m "Shapechanger Bread and Butter") then [] else
    case (getResponse m (k++"-bb")) of
      Nothing -> []
      Just atWillname -> if (not (List.member atWillname (shapeAtwillNames m k))) then []
      else [Power "Bread and Butter" ("When any power or ability allows you to make a Basic attack while you are in " ++ shape.name ++ " form, you may use " ++ atWillname ++ " instead.") Misc None 0 0 0 White]

shapeRolePowers m k =
  if (lacksFeat m "Multi-Role Shapechanger") then [] else
    case (getResponse m (k++"-role")) of
      Nothing -> []
      Just roleName -> case (Dict.get roleName roles) of
        Nothing -> []
        Just role -> role.rolePowerListPrefix m (k ++ "-") ++ [quickSpecial "Multi-Role Shapechanger" m]

shapePowerBlock m k =
  case (getResponse m k) of
    Nothing -> []
    Just shapeName -> case (Dict.get shapeName shapes) of
      Nothing -> []
      Just shape -> [{name=shapeName,powers = [shape.transform m, shape.special m, shape.atwill1 m, shape.atwill2 m] ++
        atLevel m 3 (shape.l3enc m) ++
        atLevel m 7 (shape.l7enc m) ++
        shapeBbpower m k shape ++ shapeRolePowers m k
          }]

shapeAtwillNames m k =
  case (getResponse m k) of
      Nothing -> []
      Just shapeName -> case (Dict.get shapeName shapes) of
        Nothing -> []
        Just shape -> [.name (shape.atwill1 m), .name (shape.atwill2 m)]

roles : Dict.Dict String Role
roles = Dict.fromList [("Blaster",roleBlaster),("Controller",roleController),
                     ("Defender",roleDefender),("Leader",roleLeader),("Striker",roleStriker)]


shapeChoiceField : Model -> String -> String -> (Dict.Dict String Shape) -> List Field
shapeChoiceField m name key list =
  [DropdownField { name=name, del=False, key=key, choices=[""] ++ (Dict.keys (list)) }] ++
    (if (hasFeat m "Multi-Role Shapechanger") then [DropdownField {name="Role:", del=False, key=(key++"-role"), choices=[""] ++ (Dict.keys(roles))}] else []) ++
    (if (hasFeat m "Shapechanger Bread and Butter") then [DropdownField {name="Bread and Butter:", del=False, key=(key++"-bb"), choices=[""]++(shapeAtwillNames m key)}] else [])



classShapechanger : Class
classShapechanger = { name = "Shapechanger",
               classPowerList = powers,
               classForms = forms,
               classPowerBlocks = powerBlocks,
               modifyBasicMelee = Just modifyBasicMelee,
               modifyBasicRange = Just modifyBasicRange,
               modifyRally = Just modifyRally,
               modifyCharge = Nothing,
               modifyHP = Nothing,
               classFeats = ["Shapechanger Bread and Butter","Multi-Role Shapechanger"] }


modifyBasicMelee : Model -> Power -> Power
modifyBasicMelee m p = {p | damage = atWillDamage m}

modifyBasicRange : Model -> Power -> Power
modifyBasicRange m p = {p | damage = atWillDamage m}

modifyRally : Model -> Power -> Power
modifyRally m p = case (getResponse m "shaper-type") of
  Just "One-Form" -> {p | text = overtext m "OneFormShaperRally"}
  Just "Multi-Form" -> {p | text = overtext m "MultiFormShaperRally"}
  _ -> p


powers m = [quickPower "Blurred Form" Attack AtWill 0 0 2 Green m,
            quickPower "Primal Compulsion" Attack AtWill 10 0 2 Green m] ++
            case (getResponse m "shaper-type") of
              Just "One-Form" -> [quickSpecial "One Form Shaper" m]
              Just "Multi-Form" -> [quickSpecial "Multi Form Shaper" m]
              _ -> []


powerBlocks m = case (getResponse m "shaper-type") of
  Just "One-Form" -> shapePowerBlock m "shaper-shape1"
  Just "Multi-Form" -> List.concatMap (\x -> shapePowerBlock m x) ["shaper-shape1", "shaper-shape2", "shaper-shape3"]
  _ -> []


multiRoleForm m k =
    case (getResponse m (k++"-role")) of
      Nothing -> []
      Just roleName -> case (Dict.get roleName roles) of
        Nothing -> []
        Just role -> case (getResponse m k) of
          Nothing -> []
          Just shapeName -> case (Dict.get shapeName shapes) of
            Nothing -> []
            Just shape -> List.map (\x -> {x | name = x.name ++ " (" ++ shapeName ++ ")"}) (role.roleFormsPrefix m (k ++ "-"))

multiRoleForms m = if (lacksFeat m "Multi-Role Shapechanger") then [] else
  case (getResponse m "shaper-type") of
    Just "Multi-Form" -> multiRoleForm m "shaper-shape1"
                        ++ multiRoleForm m "shaper-shape2"
                        ++ multiRoleForm m "shaper-shape3"
    Just "One-Form" -> multiRoleForm m "shaper-shape1"
    _ -> []


forms m = [Form False "Shapechanger" ([
  DropdownField {name="Type:",key="shaper-type",del=False,choices=["","Multi-Form","One-Form"]}]
  ++ case (getResponse m "shaper-type") of
    Just "Multi-Form" -> shapeChoiceField m "Form:" "shaper-shape1" shapes
                        ++ shapeChoiceField m "Form:" "shaper-shape2" shapes
                        ++ shapeChoiceField m "Form:" "shaper-shape3" shapes
    Just "One-Form" -> shapeChoiceField m "Form:" "shaper-shape1" shapes
    _ -> [])]
  ++ multiRoleForms m
