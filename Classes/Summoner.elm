module Classes.Summoner exposing (classSummoner)

import ModelDB exposing (..)
import FormsModel exposing (..)
import PowerUtilities exposing (..)
import Dict
import List
import String

type SummonType = Elemental | Fey | Angel | Demon | Greater

type alias Summon = {
   name : String,
   kind : SummonType,
   summon : (Model -> Power),
   atwill : (Model -> Power),
   creature : (Model -> Power)
 }

atWillDamage : Model -> Int
atWillDamage m = if ((getLevel m) < 5) then 2 else 3

fire = Summon "Fire Elemental" Elemental
  (quickPower "Summon Fire Elemental" Attack Encounter 0 0 3 Purple)
  (\m -> quickPower "Elemental Flare" Attack AtWill 0 0 (atWillDamage m) Green m)
  (levelTextSpecial "Fire Elemental" [1,5,9])

water = Summon "Water Elemental" Elemental
  (quickPower "Summon Water Elemental" Attack Encounter 0 0 3 Purple)
  (\m -> quickPower "Drowning" Attack AtWill 0 0 (atWillDamage m) Green m)
  (levelTextSpecial "Water Elemental" [1,5,9])

air = Summon "Air Elemental" Elemental
  (quickPower "Summon Air Elemental" Attack Encounter 0 0 3 Purple)
  (\m -> quickPower "Whirlwind" Attack AtWill 0 0 (atWillDamage m) Green m)
  (levelTextSpecial "Air Elemental" [1,5,9])

earth = Summon "Earth Elemental" Elemental
  (quickPower "Summon Earth Elemental" Attack Encounter 0 0 3 Purple)
  (\m -> quickPower "Quake" Attack AtWill 0 0 (atWillDamage m) Green m)
  (levelTextSpecial "Earth Elemental" [1,5,9])

nymph = Summon "Nymph" Fey
  (quickPower "Summon Nymph" Attack Encounter 5 0 3 Purple)
  (\m -> quickPower "Innocent Charm" Attack AtWill 10 0 (atWillDamage m) Green m)
  (levelTextSpecial "Nymph" [1,5,9])

feegles = Summon "Feegles" Fey
  (quickPower "Summon Feegles" Attack Encounter 0 0 3 Purple)
  (\m -> quickPower "Crivens!" Attack AtWill 0 0 (atWillDamage m) Green m)
  (levelTextSpecial "Feegle" [1,5,9])

wisp = Summon "Wisp" Fey
  (quickPower "Summon Wisp" Attack Encounter 5 0 3 Purple)
  (\m -> quickPower "Trick" Attack AtWill 5 0 (atWillDamage m) Green m)
  (levelTextSpecial "Wisp" [1,5,9])

redcap = Summon "Red Cap" Fey
  (quickPower "Summon Red Cap" Attack Encounter 0 0 3 Purple)
  (\m -> quickPower "Marked for Death" Attack AtWill -5 0 (atWillDamage m) Green m)
  (levelTextSpecial "Red Cap" [1,5,9])

aprot = Summon "Angel of Protection" Angel
  (quickPower "Summon Angel of Protection" Reaction Encounter 0 0 0 Purple)
  (\m -> quickPower "Protective Smite" Attack AtWill -5 0 (atWillDamage m) Green m)
  (quickSpecial "Angel of Protection")

aveng = Summon "Angel of Vengeance" Angel
  (quickPower "Summon Angel of Vengeance" Reaction Encounter 0 0 0 Purple)
  (\m -> quickPower "Vengeful Smite" Attack AtWill -5 0 (atWillDamage m) Green m)
  (quickSpecial "Angel of Vengeance")

aheal = Summon "Angel of Healing" Angel
  (quickPower "Summon Angel of Healing" Reaction Encounter 0 0 0 Purple)
  (\m -> quickPower "Healing Smite" Attack AtWill -5 0 (atWillDamage m) Green m)
  (quickSpecial "Angel of Healing")

dwrath = Summon "Daemon of Wrath" Demon
  (quickPower "Summon Daemon of Wrath" Reaction Encounter 0 0 0 Purple)
  (\m -> quickPower "Wrathful Claw" Attack AtWill 0 0 (atWillDamage m) Green m)
  (quickSpecial "Daemon of Wrath")

dsloth = Summon "Daemon of Sloth" Demon
  (quickPower "Summon Daemon of Sloth" Reaction Encounter 0 0 0 Purple)
  (\m -> quickPower "Slothful Stupor" Attack AtWill 0 0 (atWillDamage m) Green m)
  (quickSpecial "Daemon of Sloth")

dpride = Summon "Daemon of Pride" Demon
  (quickPower "Summon Daemon of Pride" Reaction Encounter 0 0 0 Purple)
  (\m -> quickPower "Prideful Arrogance" Attack AtWill 0 0 (atWillDamage m) Green m)
  (quickSpecial "Daemon of Pride")

theMother = Summon "The Mother" Greater
  (quickPower "Summon The Mother" Attack Encounter 0 0 0 Purple)
  (\m -> quickPower "Spawn" Attack AtWill 0 0 0 Green m)
  (quickSpecial "The Mother")

theVoice = Summon "The Voice" Greater
  (quickPower "Summon The Voice" Attack Encounter 0 0 4 Purple)
  (\m -> quickPower "Confusion" Attack AtWill 0 0 3 Green m)
  (quickSpecial "The Voice")

theStrangler = Summon "The Strangler" Greater
  (quickPower "Summon The Strangler" Attack Encounter 0 0 4 Purple)
  (\m -> quickPower "Spawn" Attack AtWill 0 0 3 Green m)
  (quickSpecial "The Strangler")

theContagion = Summon "Contagion" Greater
  (quickPower "Summon Contagion" Attack Encounter 0 0 4 Purple)
  (\m -> quickPower "Worsening Plague" Attack AtWill 0 0 3 Green m)
  (quickSpecial "Contagion")

allSummons = [fire, water, air, earth, nymph, feegles, wisp, redcap, aprot, aveng,
              aheal, dwrath, dsloth, dpride, theMother, theVoice, theStrangler, theContagion]

summonToPair : Summon -> (String, Summon)
summonToPair s = (s.name, s)

allSummonsDict = Dict.fromList (List.map summonToPair allSummons)

typeSummonsDict k = Dict.fromList (List.map summonToPair (List.filter (\x -> x.kind == k) allSummons))

summonTypeNames = Dict.fromList [("Elementals",Elemental),("Fey",Fey),("Angels",Angel),("Daemons",Demon)]

typePrefix t = case t of
  Elemental -> "summon-e"
  Fey -> "summon-f"
  Angel -> "summon-a"
  Demon -> "summon-d"
  Greater -> "summon-g"

singularName t = case t of
  Elemental -> "Elemental:"
  Fey -> "Faery:"
  Angel -> "Angel:"
  Demon -> "Daemon:"
  Greater -> "Greater:"

summonFieldName t ind = ((typePrefix t) ++ toString ind)

summonChoiceField m typelookup index =
  case (getResponse m typelookup) of
    Nothing -> []
    Just typeName -> case (Dict.get typeName summonTypeNames) of
      Nothing -> []
      Just summonType -> summonChoiceFieldDirect m summonType index

summonChoiceFieldDirect m summonType index =
  [DropdownField {name=(singularName summonType),del=False,key=(summonFieldName summonType index),choices=[""] ++ (Dict.keys (typeSummonsDict summonType))}]

summonPowerBlock m typelookup index =
  case (getResponse m typelookup) of
    Nothing -> []
    Just typeName -> case (Dict.get typeName summonTypeNames) of
      Nothing -> []
      Just summonType -> summonPowerBlockDirect m summonType index

summonPowerBlockDirect m summonType index =
    case (getResponse m (summonFieldName summonType index)) of
      Nothing -> []
      Just summonName -> case (Dict.get summonName allSummonsDict) of
        Nothing -> []
        Just summon -> [PowerBlock summonName [summon.summon m, summon.atwill m, summon.creature m]]

classSummoner : Class
classSummoner = { name = "Summoner",
               classPowerList = powers,
               classForms = forms,
               classPowerBlocks = powerBlocks,
               modifyBasicMelee = Nothing,
               modifyBasicRange = Nothing,
               modifyRally = Just modifyRally,
               modifyCharge = Nothing,
               modifyHP = Nothing,
               classFeats = ["Reliable Summoner"] }

modifyRally : Model -> Power -> Power
modifyRally m p =
  if ((getResponse m "summon-type1" == Just "Fey") &&
     ((getResponse m "summon-f1" == Just "Feegles") ||
      (getResponse m "summon-f2" == Just "Feegles") ||
      (getResponse m "summon-f3" == Just "Feegles"))) then
    {p | text = overtext m "SummonerRallyFeegles"}
    else
      {p | text = overtext m "SummonerRally"}

eggpower m = case (getResponse m "basics-name") of
  Nothing -> []
  Just x -> if ((String.toLower x) == "bmx bandit!") then
    [Power "You're the BMX Bandit" "You are quite good on your BMX." Misc None 0 0 0 White]
  else []

spirits m = powerDict m [
    quickPower "Punisher" Attack AtWill 10 0 (atWillDamage m) Green,
    quickPower "Scout" Attack AtWill 10 0 (atWillDamage m) Green,
    quickPower "Trooper" Attack AtWill 10 0 (atWillDamage m) Green,
    quickPower "Shadow" Attack AtWill 10 0 (atWillDamage m) Green]

powers m = [levelTextSpecial "Summoning" [1,5] m] ++
            eggpower m ++
            powerlookup m "summon-sp1" spirits

greaterSummonPowerBlock = []
greaterSummonChoiceField m i = []


powerBlocks m = summonPowerBlock m "summon-type1" 1
             ++ summonPowerBlock m "summon-type1" 2
             ++ atLevelList m 5 (summonPowerBlock m "summon-type1" 3)
             ++ atLevelList m 9 (summonPowerBlock m "summon-type1" 4)
             ++ atLevelList m 3 (summonPowerBlock m "summon-type2" 1)
             ++ atLevelList m 5 (summonPowerBlock m "summon-type2" 2)
             ++ atLevelList m 9 (summonPowerBlock m "summon-type2" 3)
             ++ atLevelList m 7 (summonPowerBlockDirect m Greater 1)
             ++ atLevelList m 9 (summonPowerBlockDirect m Greater 2)

forms m = [Form False "Summoner" ([
  powerChoiceField m "Spirit:" "summon-sp1" spirits,
  DropdownField {name="First Type:",key="summon-type1",del=False,choices=["","Elementals","Fey"]}]
  ++ summonChoiceField m "summon-type1" 1
  ++ summonChoiceField m "summon-type1" 2 ++
    atLevelList m 3 ([
      DropdownField {name="Second Type:",key="summon-type2",del=False,choices=["","Angels","Daemons"]}] ++
      summonChoiceField m "summon-type2" 1) ++
    atLevelList m 5 (
      summonChoiceField m "summon-type1" 3 ++
      summonChoiceField m "summon-type2" 2
    ) ++
    atLevelList m 7 (
      summonChoiceFieldDirect m Greater 1
    ) ++
    atLevelList m 9 (
      summonChoiceField m "summon-type1" 4 ++
      summonChoiceField m "summon-type2" 3 ++
      summonChoiceFieldDirect m Greater 2
    ))]
