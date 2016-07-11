module Classes.Shapechanger exposing (classShapechanger)

import ModelDB exposing (..)
import FormsModel exposing (..)
import PowerUtilities exposing (..)
import Dict
import List

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

shapePowers m k =
  case (getResponse m k) of
    Nothing -> []
    Just shapeName -> case (Dict.get shapeName shapes) of
      Nothing -> [quickSpecial "Undefined Shape??" m]
      Just shape -> [shape.transform m, shape.special m, shape.atwill1 m, shape.atwill2 m] ++
        atLevel m 3 (shape.l3enc m) ++
        atLevel m 7 (shape.l7enc m)


shapeChoiceField : Model -> String -> String -> (Dict.Dict String Shape) -> Field
shapeChoiceField m name key list =
  DropdownField { name=name, del=False, key=key, choices=[""] ++ (Dict.keys (list)) }

classShapechanger : Class
classShapechanger = { name = "Bombardier",
               classPowerList = powers,
               classForms = forms,
               modifyBasicMelee = Just modifyBasicMelee,
               modifyBasicRange = Just modifyBasicRange,
               modifyRally = Just modifyRally,
               modifyCharge = Nothing }


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
                 Just "One-Form" -> [quickSpecial "One Form Shaper" m] ++ shapePowers m "shaper-shape1"
                 Just "Multi-Form" -> [quickSpecial "Multi Form Shaper" m] ++
                  shapePowers m "shaper-shape1" ++ shapePowers m "shaper-shape2" ++ shapePowers m "shaper-shape3"
                 _ -> []

forms m = [Form False "Shapechanger" ([
  DropdownField {name="Type:",key="shaper-type",del=False,choices=["","Multi-Form","One-Form"]}]
  ++ case (getResponse m "shaper-type") of
    Just "Multi-Form" -> [shapeChoiceField m "Form:" "shaper-shape1" shapes,
                          shapeChoiceField m "Form:" "shaper-shape2" shapes,
                          shapeChoiceField m "Form:" "shaper-shape3" shapes]
    Just "One-Form" -> [shapeChoiceField m "Form:" "shaper-shape1" shapes]
    _ -> [])]
