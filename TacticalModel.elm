module TacticalModel exposing (..)
import Necromancer exposing (classNecro)
import Archer exposing (classArcher)
import Duelist exposing (classDuelist)
import MartialArtist exposing (classMA)
import ModelDB exposing (..)
import FormsModel exposing (..)
import Dict exposing (Dict)

{-| Returns the list of available implemented classes. -}
classes : Dict String Class
classes = Dict.fromList [("Archer",classArcher),
                         ("Duelist",classDuelist),
                         ("Martial Artist",classMA),
                         ("Necromancer",classNecro)]


pmeleeBasic : Model -> Power
pmeleeBasic m = {name = "Melee Basic Attack",
               text = "No effect.",
               slot = Attack,
               freq = AtWill,
               range = 0,
               area = 0,
               damage = 2,
               styl = Green
               }

prangedBasic : Model -> Power
prangedBasic m = {name = "Ranged Basic Attack",
               text = "No effect.",
               slot = Attack,
               freq = AtWill,
               range = 5,
               area = 0,
               damage = 2,
               styl = Green
               }

pcharge : Power
pcharge = {name = "Charge",
               text = "Move up to your speed to a square adjacent a creature, and make a Melee Basic
                       Attack against it. Each square of movement must bring you closer to the target.
                       You cannot Charge through Difficult Terrain.",
               slot = Attack,
               freq = AtWill,
               range = 0,
               area = 0,
               damage = 0,
               styl = Green
               }

pRally : Power
pRally = {name = "Rally",
               text = "No action. You may only use this on your turn, but you may use at any point
               in your turn, even while Incapacitated, Dominated, or under any other Status. Spend
               an Action Point. Regain 4 Hit Points and regain the use of one Encounter Power from your
               Class (not a Role Action) you have expended.",
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
               slot = Role,
               freq = AtWill,
               range = 0,
               area = 0,
               damage = 0,
               styl = Blue
               }

nullPowerModifier : Model -> Power -> Power
nullPowerModifier _ p = p


basicPowers : Model -> List Power
basicPowers m = [pmeleeBasic m, prangedBasic m, pcharge, pRally, pAssess]

-- indirectLookup model key db func default error =
classPowers : Model -> List Power
classPowers m = indirectLookup m "basics-class" classes (\x -> x.classPowerList m) [] []

tacticalForms : Model -> List Form
tacticalForms m = indirectLookup m "basics-class" classes (\x -> x.classForms m) [] []

getPowers : Model -> List Power
getPowers m = basicPowers m ++ classPowers m
