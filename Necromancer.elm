module Necromancer exposing (classNecro)

import ModelDB exposing (..)
import FormsModel exposing (..)
import Dict exposing (..)
import PowerUtilities exposing (..)


classNecro : Class
classNecro = { name = "Necromancer",
               classPowerList = necroPowers,
               classForms = necroForms,
               modifyBasicMeleeDamage = necroBasicMeleeDamage,
               modifyBasicRangeDamage = necroBasicRangeDamage,
               modifyBasicRangeRange = basicRangeRange }

necroBasicMeleeDamage : Model -> Int
necroBasicMeleeDamage m = if (getLevel m < 5) then 0 else 1

necroBasicRangeDamage : Model -> Int
necroBasicRangeDamage m = if (getLevel m < 5) then 0 else 1

basicRangeRange m = 0




commandUndead : Model -> Power
commandUndead m = {name = "Command Undead",
                 text = (if (getLevel m) < 9 then (overtext m "CommandUndead" "See page 102.")
                             else (overtext m "CommandUndead9+" "See pages 102 and 103.")),
                 slot = Attack,
                 freq = Encounter,
                 range = 10,
                 area = 0,
                 damage = 0,
                 styl = Purple
                }

atwilldamage m = if (getLevel m < 5) then 2 else 3


deadlyPoison : Model -> Power
deadlyPoison m = {name = "Deadly Poison",
                 text = (if (getLevel m) < 5 then (overtext m "DeadlyPoison" "See page 102.")
                                      else (overtext m "DeadlyPoison5+" "See page 102 (increased ongoing damage).")),
                 slot = Attack,
                 freq = AtWill,
                 range = -5,
                 area = 0,
                 damage = atwilldamage m,
                 styl = Green
                }

phantasms : Model -> Power
phantasms m = {name = "Phantasms",
                 text = overtext m "Phantasms" "See page 102.",
                 slot = Attack,
                 freq = AtWill,
                 range = 5,
                 area = 0,
                 damage = atwilldamage m,
                 styl = Green
                }

terrifyingVisage : Model -> Power
terrifyingVisage m = {name = "Terrifying Visage",
                 text = (if (getLevel m) < 5 then (overtext m "TerrifyingVisage" "See page 102.")
                                      else (overtext m "TerrifyingVisage5+" "See page 102 (improved).")),
                 slot = Attack,
                 freq = AtWill,
                 range = 5,
                 area = 0,
                 damage = atwilldamage m,
                 styl = Green
                }







lifeDrain : Model -> Power
lifeDrain m = {name = "Life Drain",
                 text = overtext m "LifeDrain" "See page 102.",
                 slot = Attack,
                 freq = Encounter,
                 range = 0,
                 area = 0,
                 damage = 2,
                 styl = Purple
                }

corpseExplosion : Model -> Power
corpseExplosion m = {name = "Corpse Explosion",
                 text = overtext m "CorpseExplosion" "See page 102.",
                 slot = Attack,
                 freq = Encounter,
                 range = 0,
                 area = 0,
                 damage = 0,
                 styl = Purple
                }

raiseAlly : Model -> Power
raiseAlly m = {name = "Raise Ally",
              text = overtext m "RaiseAlly" "Free action. See page 102.",
              slot = Misc,
              freq = Encounter,
              range = 0,
              area = 0,
              damage = 0,
              styl = Purple
            }


seedOfFear : Model -> Power
seedOfFear = quickPower "Seed Of Fear" 102 Attack Encounter 5 0 3 Purple


greaterMarkOfDeath : Model -> Power
greaterMarkOfDeath m = {name = "Greater Mark Of Death",
              text = case (getResponse m "necro-gift") of
                    Just "Undeath" -> overtext m "GreaterMarkOfDeathWithUndeath" "See page 102."
                    _ -> overtext m "GreaterMarkOfDeath" "See page 102.",
              slot = Attack,
              freq = Encounter,
              range = -5,
              area = 0,
              damage = 3,
              styl = Purple
            }

lichPact : Model -> Power
lichPact m = {name = "Lich Pact",
        text = overtext m "LichPact" "Reaction. See page 102.",
        slot = Reaction, freq = Encounter,
        range = 0, area = 0, damage = 0, styl = Purple}

healthSwap : Model -> Power
healthSwap m = {name = "Health Swap",
        text = overtext m "HealthSwap" "Free Action. See page 102.",
        slot = Misc, freq = Encounter,
        range = 0, area = 0, damage = 0, styl = Purple}

crudeDomination : Model -> Power
crudeDomination m = {name = "Crude Domination",
        text = overtext m "CrudeDomination" "See page 103.",
        slot = Attack, freq = Encounter,
        range = 5, area = 0, damage = 4, styl = Purple}

armyOfSpecters : Model -> Power
armyOfSpecters m = {name = "Army of Specters",
        text = overtext m "ArmyOfSpecters" "See page 103.",
        slot = Attack, freq = Encounter,
        range = 0, area = 10, damage = 0, styl = Purple}

playDiceWithDeath : Model -> Power
playDiceWithDeath m = {name = "Play Dice with Death",
        text = overtext m "PlayDiceWithDeath" "See page 103.",
        slot = Attack, freq = Encounter,
        range = 0, area = 10, damage = 0, styl = Purple}

terror : Model -> Power
terror m = {name = "Terror",
        text = overtext m "Terror" "See page 103.",
        slot = Attack, freq = Encounter,
        range = -5, area = 0, damage = 4, styl = Purple}





markOfDeath : Model -> Power
markOfDeath m = {name = "Mark of Death",
              text = overtext m "MarkOfDeath" "See page 102.",
              slot = Special,
              freq = None,
              range = 0,
              area = 0,
              damage = 0,
              styl = White
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
       text = overtext m giftTextKey ("See page " ++ giftPage ++ "."),
       slot = Special, freq = None,
       range = 0, area = 0, damage = 0, styl=White}]



l1atwills m = powerDict m [deadlyPoison, phantasms, terrifyingVisage]
l1atwillpower1 m = powerlookup m "necro-aw1" l1atwills
l1atwillpower2 m = powerlookup m "necro-aw2" l1atwills


l1encoptions m = powerDict m [lifeDrain,corpseExplosion,raiseAlly,seedOfFear]
l3encoptions m = powerDict m [greaterMarkOfDeath, lichPact, healthSwap]
l7encoptions m = powerDict m [crudeDomination, armyOfSpecters, playDiceWithDeath, terror]

l1encpower m = powerlookup m "necro-enc" l1encoptions
l3encpower m = powerlookup m "necro-enc3" l3encoptions
l7encpower m = powerlookup m "necro-enc7" l7encoptions

necroPowers m = [commandUndead m, markOfDeath m] ++
    (giftPower m) ++ (l1atwillpower1 m) ++ (l1atwillpower2 m) ++ (l1encpower m) ++
    (if ((getLevel m) >= 3) then (l3encpower m) else []) ++
    (if ((getLevel m) >= 7) then (l7encpower m) else [])

necroForm m = Form False "Necromancer" ([
  DropdownField { name="Gift", del=False, key="necro-gift", choices=["","Undeath","Terror","Vampirism"] },
  powerChoiceField m "At-Will:" "necro-aw1" l1atwills,
  powerChoiceField m "At-Will:" "necro-aw2" l1atwills,
  DropdownField { name="Encounter Power:", del=False, key="necro-enc", choices=([""] ++ keys (l1encoptions m))}
 ] ++
  (if ((getLevel m) >= 3) then
    [DropdownField { name="Encounter Power:",del=False,key="necro-enc3",choices=([""] ++ keys (l3encoptions m))}]
  else []
  ) ++
  (if ((getLevel m) >= 7) then
    [DropdownField { name="Encounter Power:",del=False,key="necro-enc7",choices=([""] ++ keys (l7encoptions m))}]
  else []
  ))

necroForms m = [necroForm m]
