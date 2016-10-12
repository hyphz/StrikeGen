module Classes.Necromancer exposing (classNecro)

import ModelDB exposing (..)
import FormsModel exposing (..)
import Dict exposing (..)
import PowerUtilities exposing (..)


classNecro : Class
classNecro = { name = "Necromancer",
               classPowerList = necroPowers,
               classForms = necroForms,
               classPowerBlocks = \m -> [],
               modifyBasicMelee = Just necroBasicMelee,
               modifyBasicRange = Just necroBasicRange,
               modifyCharge = Nothing,
               modifyRally = Nothing,
               modifyHP = Nothing,
               classFeats = [] }

necroBasicMelee : Model -> Power -> Power
necroBasicMelee m p = if (getLevel m < 5) then p else {p | damage = 3}


necroBasicRange : Model -> Power -> Power
necroBasicRange m p = if (getLevel m < 5) then p else {p | damage = 3}




commandUndead : Model -> Power
commandUndead m = {name = "Command Undead",
                 text = (if (getLevel m) < 9 then (overtext m "CommandUndead")
                             else (overtext m "CommandUndead9+")),
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
                 text = (overtext m "DeadlyPoison"),
                 slot = Attack,
                 freq = AtWill,
                 range = -5,
                 area = 0,
                 damage = atwilldamage m,
                 styl = Green
                }

phantasms : Model -> Power
phantasms m = {name = "Phantasms",
                 text = overtext m "Phantasms",
                 slot = Attack,
                 freq = AtWill,
                 range = 5,
                 area = 0,
                 damage = atwilldamage m,
                 styl = Green
                }

terrifyingVisage : Model -> Power
terrifyingVisage m = {name = "Terrifying Visage",
                 text = (overtext m "TerrifyingVisage"),
                 slot = Attack,
                 freq = AtWill,
                 range = 5,
                 area = 0,
                 damage = atwilldamage m,
                 styl = Green
                }


lifeDrain = quickPower "Life Drain" Attack Encounter 0 0 2 Purple
corpseExplosion = quickPower "Corpse Explosion" Attack Encounter 0 0 0 Purple
raiseAlly = quickPower "Raise Ally" Misc Encounter 0 0 0 Purple

seedOfFear : Model -> Power
seedOfFear = quickPower "Seed Of Fear" Attack Encounter 5 0 3 Purple


greaterMarkOfDeath : Model -> Power
greaterMarkOfDeath m = {name = "Greater Mark Of Death",
              text = case (getResponse m "necro-gift") of
                    Just "Undeath" -> overtext m "GreaterMarkOfDeathWithUndeath"
                    _ -> overtext m "GreaterMarkOfDeath",
              slot = Attack,
              freq = Encounter,
              range = -5,
              area = 0,
              damage = 3,
              styl = Purple
            }

lichPact : Model -> Power
lichPact m = {name = "Lich Pact",
        text = overtext m "LichPact",
        slot = Reaction, freq = Encounter,
        range = 0, area = 0, damage = 0, styl = Purple}

healthSwap : Model -> Power
healthSwap m = {name = "Health Swap",
        text = overtext m "HealthSwap",
        slot = Misc, freq = Encounter,
        range = 0, area = 0, damage = 0, styl = Purple}

crudeDomination : Model -> Power
crudeDomination m = {name = "Crude Domination",
        text = overtext m "CrudeDomination",
        slot = Attack, freq = Encounter,
        range = 5, area = 0, damage = 4, styl = Purple}

armyOfSpecters : Model -> Power
armyOfSpecters m = {name = "Army of Specters",
        text = overtext m "ArmyOfSpecters",
        slot = Attack, freq = Encounter,
        range = 0, area = 10, damage = 0, styl = Purple}

playDiceWithDeath : Model -> Power
playDiceWithDeath m = {name = "Play Dice with Death",
        text = overtext m "PlayDiceWithDeath",
        slot = Attack, freq = Encounter,
        range = 0, area = 10, damage = 0, styl = Purple}

terror : Model -> Power
terror m = {name = "Terror",
        text = overtext m "Terror",
        slot = Attack, freq = Encounter,
        range = -5, area = 0, damage = 4, styl = Purple}





markOfDeath : Model -> Power
markOfDeath m = {name = "Mark of Death",
              text = overtext m "MarkOfDeath",
              slot = Special,
              freq = None,
              range = 0,
              area = 0,
              damage = 0,
              styl = White
            }

speakWithDead : Model -> Power
speakWithDead m = quickPower "Speak With Dead" Attack AtWill -5 0 (atwilldamage m) Green m




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
       text = overtext m giftTextKey,
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

necroPowers m = [commandUndead m, markOfDeath m, speakWithDead m] ++
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
