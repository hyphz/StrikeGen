module Necromancer exposing (classNecro)

import ModelDB exposing (..)
import FormsModel exposing (..)
import Dict exposing (..)


classNecro : Class
classNecro = { name = "Necromancer",
               classPowerList = necroPowers,
               classForms = necroForms,
               modifyBasicDamage = necroBasicDamage }
necroBasicDamage : Model -> Int
necroBasicDamage m = if (getLevel m < 5) then 0 else 1



commandUndead : Power
commandUndead = {name = "Command Undead",
                 text = overtext "CommandUndead" "See page 102.",
                 slot = Attack,
                 freq = Encounter,
                 range = 10,
                 area = 0,
                 damage = 0
                }

lifeDrain : Power
lifeDrain = {name = "Life Drain",
                 text = overtext "LifeDrain" "See page 102.",
                 slot = Attack,
                 freq = Encounter,
                 range = 0,
                 area = 0,
                 damage = 2
                }

corpseExplosion : Power
corpseExplosion = {name = "Corpse Explosion",
                 text = overtext "CorpseExplosion" "See page 102.",
                 slot = Attack,
                 freq = Encounter,
                 range = 0,
                 area = 0,
                 damage = 0
                }

raiseAlly : Power
raiseAlly = {name = "Raise Ally",
              text = overtext "RaiseAlly" "Free action. See page 102.",
              slot = Misc,
              freq = Encounter,
              range = 0,
              area = 0,
              damage = 0
            }

seedOfFear : Power
seedOfFear = {name = "Seed Of Fear",
              text = overtext "SeedOfFear" "See page 102.",
              slot = Attack,
              freq = Encounter,
              range = 5,
              area = 0,
              damage = 3
            }

greaterMarkOfDeath : Power
greaterMarkOfDeath = {name = "Greater Mark Of Death",
              text = overtext "GreaterMarkOfDeath" "See page 102.",
              slot = Attack,
              freq = Encounter,
              range = -5,
              area = 0,
              damage = 3
            }


markOfDeath : Power
markOfDeath = {name = "Mark of Death",
              text = overtext "MarkOfDeath" "See page 102.",
              slot = Special,
              freq = None,
              range = 0,
              area = 0,
              damage = 0
            }




giftPower m = case (getResponse m "necro-gift") of
  Nothing -> []
  Just gift ->
    let
      giftLevel =
        if (getLevel m) < 5 then 1 else
          if (getLevel m) < 9 then 2 else 3
      giftLevelDesc = case giftLevel of
        1 -> "Lesser"
        2 -> ""
        3 -> "Greater"
        _ -> "Broken"
      giftTextKey = giftLevelDesc ++ "GiftOf" ++ gift
      giftName = giftLevelDesc ++ " Gift Of " ++ gift
      giftPage = case giftLevel of
        1 -> "102"
        2 -> "102"
        3 -> "103"
        _ -> "XX"
    in
      [{name = giftName,
       text = overtext giftTextKey ("See page " ++ giftPage ++ "."),
       slot = Special, freq = None,
       range = 0, area = 0, damage = 0}]




l1encoptions = Dict.fromList [("Life Drain",lifeDrain),
                ("Corpse Explosion",corpseExplosion),
                ("Raise Ally",raiseAlly),
                ("Seed of Fear",seedOfFear)]

l3encoptions = Dict.fromList [("Greater Mark of Death",greaterMarkOfDeath)]

l1encpower m = case (getResponse m "necro-enc") of
  Nothing -> []
  Just enc -> case (get enc l1encoptions) of
    Nothing -> []
    Just p -> [p]

l3encpower m = case (getResponse m "necro-enc3") of
  Nothing -> []
  Just enc -> case (get enc l3encoptions) of
    Nothing -> []
    Just p -> [p]



necroPowers m = [commandUndead, markOfDeath] ++
    (giftPower m) ++ (l1encpower m) ++
    if ((getLevel m) >= 3) then (l3encpower m) else []





necroForm m = Form False "Necromancer" ([
  DropdownField { name="Gift", del=False, key="necro-gift", choices=["","Undeath","Terror","Vampirism"] },
  DropdownField { name="Encounter Power:", del=False, key="necro-enc", choices=([""] ++ keys l1encoptions)}
 ] ++
  if ((getLevel m) >= 3) then
    [DropdownField { name="Encounter Power:",del=False,key="necro-enc3",choices=([""] ++ keys l3encoptions)}]
  else []
  )

necroForms m = [necroForm m]
