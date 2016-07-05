module TacticalModel exposing (..)
import Necromancer exposing (classNecro)
import Archer exposing (classArcher)
import Duelist exposing (classDuelist)
import MartialArtist exposing (classMA)
import Simplified exposing (classSimplified)
import Warlord exposing (classWarlord)
import Magician exposing (classMagician)

import ModelDB exposing (..)
import FormsModel exposing (..)
import Dict exposing (Dict)

{-| Returns the list of available implemented classes. -}
classes : Dict String Class
classes = Dict.fromList [("Archer",classArcher),
                         ("Duelist",classDuelist),
                         ("Martial Artist",classMA),
                         ("Magician",classMagician),
                         ("Necromancer",classNecro),
                         ("Simplified",classSimplified),
                         ("Warlord",classWarlord)]

roles = Dict.fromList []



applyClassModifier m extractor value =
  let c = indirectLookup m "basics-class" classes (\x -> (extractor x)) Nothing Nothing in
  case c of
    Nothing -> value
    Just func -> func m value





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

pAssess : Power
pAssess = {name = "Assess",
               text = "Roll a die and ask the GM that many questions as listed on page 90.",
               slot = RoleSlot,
               freq = AtWill,
               range = 0,
               area = 0,
               damage = 0,
               styl = Blue
               }

nullPowerModifier : Model -> Power -> Power
nullPowerModifier _ p = p


basicPowers : Model -> List Power
basicPowers m = [pmeleeBasic m, prangedBasic m, pcharge m, pRally m, pAssess]

-- indirectLookup model key db func default error =
classPowers : Model -> List Power
classPowers m = indirectLookup m "basics-class" classes (\x -> x.classPowerList m) [] []

tacticalForms : Model -> List Form
tacticalForms m = indirectLookup m "basics-class" classes (\x -> x.classForms m) [] []

getPowers : Model -> List Power
getPowers m = basicPowers m ++ classPowers m
