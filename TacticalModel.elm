module TacticalModel exposing (..)
import Classes.Necromancer exposing (classNecro)
import Classes.Archer exposing (classArcher)
import Classes.Duelist exposing (classDuelist)
import Classes.MartialArtist exposing (classMA)
import Classes.Simplified exposing (classSimplified)
import Classes.Warlord exposing (classWarlord)
import Classes.Magician exposing (classMagician)
import Classes.Bombardier exposing (classBombardier)
import Classes.Shapechanger exposing (classShapechanger)
import Roles.Striker exposing (roleStriker)
import Roles.Leader exposing (roleLeader)
import Roles.Defender exposing (roleDefender)
import Roles.Controller exposing (roleController)
import Roles.Blaster exposing (roleBlaster)
import ModelDB exposing (..)
import FormsModel exposing (..)
import Dict exposing (Dict)

{-| Returns the list of available implemented classes. -}
classes : Dict String Class
classes = Dict.fromList [("Archer",classArcher),
                         ("Bombardier",classBombardier),
                         ("Duelist",classDuelist),
                         ("Martial Artist",classMA),
                         ("Magician",classMagician),
                         ("Necromancer",classNecro),
                         ("Shapechanger",classShapechanger),
                         ("Simplified",classSimplified),
                         ("Warlord",classWarlord)
                         ]

{-| Returns the list of available implemented roles. -}
roles : Dict String Role
roles = Dict.fromList [("Blaster",roleBlaster),("Controller",roleController),
                     ("Defender",roleDefender),("Leader",roleLeader),("Striker",roleStriker)]


{-| Looks up the current class and applies a function (probably a field extractor) to it.
That function is expected to return another function or Nothing. If it returns a function,
apply it to the given value. -}
applyClassModifier : Model -> (Class -> Maybe (Model -> a -> a)) -> a -> a
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

classPowerBlocks : Model -> List PowerBlock
classPowerBlocks m = indirectLookup m "basics-class" classes (\x -> x.classPowerBlocks m) [] []

allClassPowers : Model -> List Power
allClassPowers m = classPowers m ++ List.concatMap .powers (classPowerBlocks m)

rolePowers : Model -> List Power
rolePowers m = indirectLookup m "basics-role" roles (\x -> x.rolePowerList m) [] []

tacticalForms : Model -> List Form
tacticalForms m = (indirectLookup m "basics-class" classes (\x -> x.classForms m) [] []) ++
                  (indirectLookup m "basics-role" roles (\x -> x.roleForms m) [] [])

getPowers : Model -> List Power
getPowers m = basicPowers m ++ classPowers m ++ rolePowers m

getPowerBlocks : Model -> List PowerBlock
getPowerBlocks m = classPowerBlocks m

getAllPowers : Model -> List Power
getAllPowers m = basicPowers m ++ allClassPowers m ++ rolePowers m
