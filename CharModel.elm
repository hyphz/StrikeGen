module CharModel exposing (..)

import List exposing (map, head, length, filter, sortBy)
import Result exposing (withDefault)
import ModelDB exposing (..)
import Dict exposing (Dict, insert)
import String exposing (toInt, concat)
import FormsModel exposing (..)
import Ports exposing (download, saveURL)
import Json.Encode exposing (encode)
import Json.Decode
import TacticalModel exposing (classes, roles, tacticalForms)

{-| ELM Architecture Initialization Function. -}
init : (Model, Cmd Msg)
init = ({character = blankCharacter,
         database = blankDatabase},
         getJsonFileCommand "data/backgrounds.json" BackgroundsLoaded)


{-| Reads the character's background skill list. -}
backgroundSkills : Model -> List String
backgroundSkills m = indirectLookup m "basics-bg" m.database.backgrounds
  .skillNames [] ["Missing background"]

{-| Reads the character's origin skill list. -}
originSkills : Model -> List String
originSkills m = indirectLookup m "basics-origin" m.database.origins
  .skillNames [] ["Missing origin"]

{-| Reads the character's origin complication list. -}
originComplications : Model -> List String
originComplications m = indirectLookup m "basics-origin" m.database.origins
  .complications [] ["Missing origin"]

-- Origins can be a bit of a nuisance because some of them have more skills and
-- complications than they're supposed to have, and the player is supposed to
-- choose which ones to go for. So we have to offer a choice if we have one of
-- these.

{-| Is an origin complex and if so, why? -}
complexOrigin : Origin -> (Bool, Bool, Bool, Bool)
complexOrigin x = ((length x.skillNames) > 2, (length x.complications) > 1,
  x.freeformSkill, x.freeformComplication)

{-| Is this model's origin conmplex and if so, why? -}
hasComplexOrigin : Model -> (Bool, Bool, Bool, Bool)
hasComplexOrigin m = indirectLookup m "basics-origin" m.database.origins
  complexOrigin (False, False, False, False) (False, False, False, False)

{-| Form for making the choices resulting from having a complex origin. -}
complexOriginForm : Model -> Maybe Form
complexOriginForm m = case (hasComplexOrigin m) of
  (False, False, False, False) -> Nothing
  (complexSkills, complexComplications, freeformSkill, freeformComplication) ->
    let
      skillPart = case complexSkills of
        True -> [DropdownField {name="First Skill:",del=False, key="origin-s1",choices=([""] ++ originSkills m)},
                 DropdownField {name="Second Skill:",del=False,key="origin-s2",choices=([""] ++ originSkills m)}]
        False -> []
      complicationPart = case complexComplications of
        True -> [DropdownField {name="Complication:",del=False,key="origin-co",choices=([""] ++ originComplications m)}]
        False -> []
      freeformSkillPart = case freeformSkill of
        True -> [FreeformField {name="Custom skill:",del=False,key="origin-cs"}]
        False -> []
      freeformComplicationPart = case freeformComplication of
        True -> [FreeformField {name="Complication:", del=False, key="origin-cco"}]
        False -> []
      originName = case getResponse m "basics-origin" of
        Just x -> x
        Nothing -> "(BUG) Origin complex but missing"
    in
      Just (Form False originName (skillPart ++ complicationPart ++ freeformSkillPart ++ freeformComplicationPart))

{-| Gets the two skills our origin actually contributes, based on the list
and/or choices. -}
resolvedOriginSkills : Model -> List String
resolvedOriginSkills m = case (hasComplexOrigin m) of
  (False, _, False, _) -> originSkills m
  (True, _, False, _) -> mayList (Dict.get "origin-s1" m.character) ++
               mayList (Dict.get "origin-s2" m.character)
  (False, _, True, _) -> originSkills m ++ mayList(Dict.get "origin-cs" m.character)
  (True, _, True, _) -> mayList (Dict.get "origin-s1" m.character) ++
               mayList (Dict.get "origin-cs" m.character)

{-| Gets the list of character skills. -}
getSkills : Model -> List Skill
getSkills m = map (skillNameToSkill 0) (resolvedBackgroundSkills m) -- Background
          ++ map (skillNameToSkill 1) (resolvedOriginSkills m) -- Origin
          ++ mayList (Maybe.map (skillNameToSkill 2) (getResponse m "basics-skill"))

{-| Turns a skill name from the database into a skill storable on the character,
setting the source and name according to the parameters. -}
skillNameToSkill : Int -> String -> Skill
skillNameToSkill source name = { name = name, source = source }

{-| Form giving the standard prompts from the book for selecting skills for a custom
background. -}
customBackgroundForm : Model -> Maybe Form
customBackgroundForm m =
  case getResponse m "basics-bg" of
    Nothing -> Nothing
    Just "<Custom>" -> Just (Form False "Custom background" ([
       FreeformField {name="Skill you need the most?", key="bg-custom-s1", del=False},
       FreeformField {name="Supporting skill or knowledge?", key="bg-custom-s2", del=False},
       FreeformField {name="Social/business skill?", key="bg-custom-s3", del=False},
       DropdownField {name="What helps when times are tough?", del=False, key="bg-custom-wos1", choices=["","Wealth","People"]}]
       ++ mayList (
            case getResponse m "bg-custom-wos1" of
              Just "People" -> Just <| FreeformField {name="Who?", del=False, key="bg-custom-wos1s"}
              _ -> Nothing
            )
       ++ [DropdownField {name="How do you get stuff?", del=False, key="bg-custom-wos2", choices=["","Money","Skill"]}]
       ++ mayList (
            case getResponse m "bg-custom-wos2" of
              Just "Skill" -> Just <| FreeformField {name="What skill?", del=False, key="bg-custom-wos2s"}
              _ -> Nothing
            )
        ++ [FreeformField {name="Trick?", key="bg-custom-t", del=False}]))
    Just _ -> Nothing


{-| Returns the skill choices for the background, allowing for it being custom.  -}
resolvedBackgroundSkills : Model -> List String
resolvedBackgroundSkills m = case getResponse m "basics-bg" of
  Nothing -> []
  Just "<Custom>" -> (mayList <| getResponse m "bg-custom-s1") ++
                     (mayList <| getResponse m "bg-custom-s2") ++
                     (mayList <| getResponse m "bg-custom-s3") ++
                      if (getResponse m "bg-custom-wos1" == Just "People") then mayList <| getResponse m "bg-custom-wos1s" else [] ++
                      if (getResponse m "bg-custom-wos2" == Just "Skill") then mayList <| getResponse m "bg-custom-wos2s" else []
  Just _ -> backgroundSkills m


{-| Quick function for removing a field value that's out of range, if it
exists. -}
killOutOfRange : String -> List String -> Model -> Model
killOutOfRange field list model =
  case getResponse model field of
    Nothing -> model
    Just val -> case List.member val list of
      True -> model
      False -> killResponse model field

{-| When origin is changed, if it's changed to a new complex origin,
remove any skill choices made for a previous complex origin that aren't
relevant to the new one. -}
validateAlteredOrigin : Model -> Model
validateAlteredOrigin m =
  case hasComplexOrigin m of
    (False, False, False, False) -> m
    _ -> (killOutOfRange "origin-s1" (originSkills m)
           (killOutOfRange "origin-s2" (originSkills m)
             (killOutOfRange "origin-co" (originComplications m) m)))


{-| Validate the level setting. This shouldn't ever become invalid, but
better safe than sorry, I guess.. -}
validateLevel : Model -> Model
validateLevel m =
  case getResponse m "basics-level" of
    Nothing -> setResponse m "basics-level" "1"
    Just x -> case (toInt x) of
      Err _ -> setResponse m "basics-level" "1"
      Ok l -> if l < 1 then setResponse m "basics-level" "1" else
              if l > 10 then setResponse m "basics-level" "10" else m

{-| Performs validation when a field changes. -}
fieldChanged : String -> Model -> Model
fieldChanged field m =
  case field of
    "basics-origin" -> validateAlteredOrigin m
    "basics-level" -> validateLevel m
    _ -> m

{-| Creates the form entry for the Xth learned skill. -}
learnedSkillEntry : Model -> Int -> Field
learnedSkillEntry m x = FreeformField{name="Skill",del=True,key="ls-"++(toString x)}

learnedSkillsForm : Model -> Form
learnedSkillsForm m = Form True "Learned Skills"
                (map (learnedSkillEntry m) [1..(getResponseInt m "ls-count" 0)])


{-| Form with the most basic common parts of a character. -}
basicsForm : Model -> Form
basicsForm model = Form False "The Basics" [
  DropdownField {name="Background", del=False, key="basics-bg", choices=(["<Custom>"] ++ (Dict.keys model.database.backgrounds))},
  DropdownField {name="Origin", del=False, key="basics-origin", choices=(Dict.keys model.database.origins)},
  NumberField {name="Level", del=False, key="basics-level", min=1, max=10},
  DropdownField {name="Class", del=False, key="basics-class", choices=([""] ++ Dict.keys classes)},
  DropdownField {name="Role", del=False, key="basics-role", choices=([""] ++ Dict.keys roles)},
  FreeformField {name="Custom Skill:", del=False, key="basics-skill"},
  FreeformField {name="Custom Trick:", del=False, key="basics-trick"},
  FreeformField {name="Complication:", del=False, key="basics-comp"}]

{-| Form holding the values set as a character advances in level. -}
levelForm : Model -> Form
levelForm model = Form False "Advancement" (
  (if ((getLevel model) >= 2) then [FreeformField {name="Fallback", del=False, key="adv-fallback"},
                                    FreeformField {name="Trick", del=False, key="adv-l2trick"}] else []) ++
  (if ((getLevel model) >= 4) then [FreeformField {name="Complication", del=False, key="adv-l4comp"}] else []) ++
  (if ((getLevel model) >= 6) then [FreeformField {name="Trick", del=False, key="adv-l6trick"}] else []) ++
  (if ((getLevel model) >= 8) then [FreeformField {name="Complication", del=False, key="adv-l8comp"}] else []) ++
  (if ((getLevel model) >= 10) then [FreeformField {name="Trick", del=False, key="adv-l10trick"}] else [])
  )

{-| Get the active forms. -}
getForms : Model -> List Form
getForms model =
   [basicsForm model] ++
   [learnedSkillsForm model] ++
   (if ((getLevel model) >= 2) then [levelForm model] else []) ++
   (mayList (complexOriginForm model)) ++
   (mayList (customBackgroundForm model)) ++
   tacticalForms model





{-| Update a field on the character when a form value changes, and call validation. -}
updateFieldResponse : String -> String -> Model -> Model
updateFieldResponse key value model =
  fieldChanged key (setResponse model key value)

extendForm : String -> Model -> Model
extendForm prefix model =
  let
    oldCount = (getResponseInt model (prefix ++ "count") 0)
    newCount = (toString (oldCount + 1))
    countUpdated = setResponse model (prefix ++ "count") newCount
    newMemberKey = prefix ++ newCount
  in
    setResponse countUpdated newMemberKey ""


{-| Called when an add button on an addable form is pressed. -}
updateExtendForm : String -> Model -> Model
updateExtendForm key model =
  case key of
    "Learned Skills" -> extendForm "ls-" model
    _ -> model



closeGaps' : String -> Int -> Int -> Model -> Model
closeGaps' prefix current total m =
  let
    addPrefix t = prefix ++ (toString t)
  in
    if current > total then
      m
    else
      case (Dict.get (addPrefix current) m.character) of
        Just _ -> closeGaps' prefix (current+1) total m
        Nothing -> case (Dict.get (addPrefix (current+1)) m.character) of
          Just _ -> closeGaps' prefix current total (moveResponse m (addPrefix (current+1)) (addPrefix current))
          Nothing -> Debug.crash ("Two missing items in CLosegaps', current is " ++ (toString current) ++ "prefix is" ++ prefix ++ "total is" ++ (toString total))

closeGaps : String -> Int -> Model -> Model
closeGaps prefix total m = closeGaps' prefix 1 total m


updateDeleteField : String -> Model -> Model
updateDeleteField key model =
  let
    keyhyphenindex = String.indexes "-" key
    realKeyHyphenIndex = case (List.head keyhyphenindex) of
      Nothing -> Debug.crash "Missing hyphen in expandable form deletion process"
      Just x -> x
    keyPrehyphen = (String.slice 0 realKeyHyphenIndex key) ++ "-"
    countName = Debug.log "name of the count variable:" (keyPrehyphen ++ "count")
    removeResponse = killResponse model key
    checkResponseRemoved = case (Dict.get key removeResponse.character) of
      Nothing -> removeResponse
      Just _ -> Debug.crash ("RemoveResponse didn't work!???")
    reduceCount = Debug.log "reduce the count:" (setResponse checkResponseRemoved countName (toString ((getResponseInt checkResponseRemoved countName 0) - 1)))
      in closeGaps keyPrehyphen (getResponseInt reduceCount countName 0) reduceCount



{-| Encodes the character hash as a Json string. -}
encodeChar : Model -> String
encodeChar model =
  model.character
  |> Dict.toList
  |> map (\(x,y) -> (x,Json.Encode.string y))
  |> Json.Encode.object
  |> Json.Encode.encode 2

{-| Deal with clicks on the "file menu". Most of these have to go to JS. -}
handleFileCommand : String -> Model -> (Model, Cmd Msg)
handleFileCommand x m = case x of
  "download" -> (m, Ports.download ("character.json",encodeChar m))
  "upload" -> (m, Ports.doUpload 0)
  "seturl" -> (m, Ports.saveURL (encodeChar m))
  "reset" -> ({m | character = blankCharacter},Ports.resetFileMenu 0)
  "roll20" -> (m, Ports.download ("macros.txt",powerMacros m))
  _ -> (m, Ports.alert ("Invalid message " ++ x ++ " from file menu"))


{-| Decode a character hash from a JSON string. -}
importChar : String -> Model -> (Model, Cmd Msg)
importChar x m =
  let
    newChar = Json.Decode.decodeString (Json.Decode.dict Json.Decode.string) x
  in
    case newChar of
      Ok chardata -> ({ m | character = chardata }, Cmd.none)
      Err string -> (m, Ports.alert "Error decoding character file.")


{-| Add spaces at carriage returns in a string, used to format markdown
strings from the database for plain text viewing. -}
tidyEdges : String -> String
tidyEdges s = s |> String.split "\n" |> String.join " "

{-| Return all parts of a string EXCEPT the selected area. -}
unSlice : Int -> Int -> String -> String
unSlice start end str =
  (String.slice 0 start str) ++ (String.slice end -1 str)


{-| Select a headed paragraph from the markdown text in the text database, and
extract it, removing the paragraph header. Returns the extracted paragraph and
the text with the extracted paragraph eliminated. -}
textExtract : String -> String -> (Maybe String, String)
textExtract section text =
  let
    start = List.head (String.indexes ("**" ++ section ++":**") text)
    possEnds = (String.indexes "\n\n" text) ++ (String.indexes "\r\n\r\n" text) ++ [String.length text]
    realEnd = case start of
      Nothing -> Nothing
      Just st -> List.head (List.filter (\x -> x > st) possEnds)
    offset = (String.length section) + 5
  in
    case start of
      Nothing -> (Nothing, text)
      Just s -> case realEnd of
        Nothing -> (Nothing, text)
        Just en -> (Just (tidyEdges (String.slice (s+offset) en text)), unSlice s en text)

{-| Perform textExtract and create a roll20 macro component if the named section was
found, or a blank string if it wasn't. -}
extractToMacroPart : String -> String -> (String, String)
extractToMacroPart name string =
  case textExtract name string of
    (Nothing, x) -> ("",x)
    (Just text, x) -> ("{{" ++ name ++"=" ++ text ++ "}}",x)

{-| Export a power as a Roll20 macro. -}
powerMacro : Power -> String
powerMacro power =
  let
    rangeDesc = case power.slot of
      Attack -> if power.range == 0 then "{{Range=Melee}}"
                 else if power.range > 0 then ("{{Range=Ranged " ++ toString power.range++"}}")
                 else ("{{Range=Melee or Ranged " ++ toString power.range++"}}")
      _ -> if power.range > 0 then ("{{Range=Ranged " ++ toString power.range ++"}}")
           else if power.range < 0 then ("{{Range=Adjacent or ranged " ++ toString (-power.range)++"}}")
           else ""
    typeDesc = case power.slot of
            Attack -> "{{Type=Attack}}"
            RoleSlot -> "{{Type=Role}}"
            Reaction -> "{{Type=Reaction}}"
            _ -> ""
    attackRoll = case power.slot of
            Attack -> "{{Attack=[[1d6]]}}"
            _ -> ""
    areaDesc = case power.area of
            0 -> ""
            _ -> "{{Area=" ++ (toString power.area) ++ "}}"
    damageDesc = case power.damage of
            0 -> ""
            (-1) -> "{{Damage=Support}}"
            _ -> "{{Damage=" ++ (toString power.damage) ++ "}}"
    (triggerDesc, triggerRest) = extractToMacroPart "Trigger" power.text
    (effectDesc, effectRest) = extractToMacroPart "Effect" triggerRest
    (specialDesc, specialRest) = extractToMacroPart "Special" effectRest
    (martialDesc, martialRest) = extractToMacroPart "Melee Effect" specialRest
    (marconDesc, marconRest) = extractToMacroPart "Continuous" martialRest
    restText = if (effectRest /= "") then "{{Text=" ++ tidyEdges marconRest ++ "}}" else ""
  in
    "&{template:default}{{name=" ++ power.name ++ "}}" ++
                       typeDesc ++
                       rangeDesc ++
                       areaDesc ++
                       attackRoll ++
                       damageDesc ++
                       triggerDesc ++
                       effectDesc ++
                       specialDesc ++
                       martialDesc ++
                       marconDesc ++
                         restText ++ "\r\n\r\n"

{-| Create the power macro data file for export. -}
powerMacros : Model -> String
powerMacros model =
  String.concat (List.map powerMacro (TacticalModel.getPowers model))



update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    FormFieldUpdated k s -> (updateFieldResponse k s model, Cmd.none)
    FormAddClicked f -> (updateExtendForm f model, Cmd.none)
    FieldDeleteClicked f -> (updateDeleteField f model, Cmd.none)
    FileCommand x -> handleFileCommand x model
    LoadJson x -> importChar x model
    _ -> dbUpdate msg model
