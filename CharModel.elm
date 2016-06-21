module CharModel exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (map, head, length, filter, sortBy)
import Html.App as Html
import Json.Decode exposing (Decoder, decodeString, (:=), object3, object4, string, list, int, at)
import Http exposing (getString, Error)
import Task exposing (perform)
import Result exposing (withDefault)
import Types exposing (..)
import Dict exposing (Dict, insert)
import String exposing (toInt)

init = ({character = blankCharacter,
         database = blankDatabase},
         getJsonFileCommand "data/backgrounds.json" BackgroundsLoaded)


mayList x = case x of
  Nothing -> []
  Just y -> [y]


indirectLookup : Model -> String -> Dict String a -> (a -> b) -> b -> b -> b
indirectLookup model key db func default error =
  case Dict.get key model.character.formresponses of
    Nothing -> default
    Just x -> case Dict.get x db of
      Nothing -> default
      Just o -> func o


backgroundSkills : Model -> List String
backgroundSkills m = indirectLookup m "basics-bg" m.database.backgrounds
  .skillNames [] ["Missing background"]

originSkills : Model -> List String
originSkills m = indirectLookup m "basics-origin" m.database.origins
  .skillNames [] ["Missing origin"]

complexOrigin : Origin -> (Bool, Bool)
complexOrigin x = ((length x.skillNames) > 2, (length x.complications) > 1)

hasComplexOrigin : Model -> (Bool, Bool)
hasComplexOrigin m = indirectLookup m "basics-origin" m.database.origins
  complexOrigin (False, False) (False, False)

complexOriginForm : Model -> Maybe Form
complexOriginForm m = case (hasComplexOrigin m) of
  (False, False) -> Nothing
  (complexSkills, complexComplications) ->
    let
      skillPart = case complexSkills of
        True -> [DropdownField {name="First Skill:",key="origin-s1",choices=([""] ++ originSkills m)},
                 DropdownField {name="Second Skill:",key="origin-s2",choices=([""] ++ originSkills m)}]
        False -> []
      complicationPart = []
    in
      Just (makeForm "Origin" (skillPart ++ complicationPart))

resolvedOriginSkills : Model -> List String
resolvedOriginSkills m = case (hasComplexOrigin m) of
  (False, _) -> originSkills m
  (True, _) -> mayList (Dict.get "origin-s1" m.character.formresponses) ++
               mayList (Dict.get "origin-s2" m.character.formresponses)


getSkills : Model -> List Skill
getSkills m = map (skillNameToSkill 0) (backgroundSkills m)
          ++ map (skillNameToSkill 1) (resolvedOriginSkills m)

{-| Turns a skill name from the database into a skill storable on the character,
setting the source and name according to the parameters. -}
skillNameToSkill : Int -> String -> Skill
skillNameToSkill source name = { name = name, source = source }


updateCharacter : (a -> Character -> Character) -> a -> Model -> Model
updateCharacter updater updata model = {model | character = (updater updata model.character) }


{-| Returns the command to load a JSON data file. If it loads successfully, send the
specified message. If it doesn't, send HTTPLoadError.  -}
getJsonFileCommand : String -> (String -> Msg) -> Cmd Msg
getJsonFileCommand fileName signal =
   perform HTTPLoadError signal (getString fileName)


updateDatabase : (Database -> Database) -> Model -> Model
updateDatabase updater model = {model | database = (updater model.database) }


httpError : Model -> Model
httpError model =  model


originsDecoder : Decoder (List Origin)
originsDecoder = "origins" := (Json.Decode.list originDecoder)

originDecoder : Decoder Origin
originDecoder =
  object4 Origin
    ("name" := string)
    ("skillNames" := Json.Decode.list string)
    ("wealth" := int)
    ("complications" := Json.Decode.list string)

backgroundsDecoder : Decoder (List Background)
backgroundsDecoder = "backgrounds" := (Json.Decode.list backgroundDecoder)

backgroundDecoder : Decoder Background
backgroundDecoder =
  object4 Background
    ("name" := string)
    ("skillNames" := Json.Decode.list string)
    ("wealth" := int)
    ("trick" := string)


toDict : (a -> comparable) -> List a -> Dict comparable a
toDict keygetter list = Dict.fromList (map (\s -> (keygetter s, s)) list)

unpackBackgrounds : String -> Model -> Model
unpackBackgrounds s model = updateDatabase ( \d ->
 { d | backgrounds = toDict .name ([nullBackground] ++ (withDefault [] (decodeString backgroundsDecoder s)))}) model

unpackOrigins : String -> Model -> Model
unpackOrigins s model = updateDatabase ( \d ->
 { d | origins = toDict .name ([nullOrigin] ++ (withDefault [] (decodeString originsDecoder s)))}) model

setResponse model key value =
  let
    char = model.character
    resp = char.formresponses
  in
   { model | character = { char | formresponses = Dict.insert key value resp }}

killResponse model key =
  let
    char = model.character
    resp = char.formresponses
  in
   { model | character = { char | formresponses = Dict.remove key resp }}

getResponse model key =
  Dict.get key model.character.formresponses


updateFieldResponse key value model =
  fieldChanged key (setResponse model key value)


validateAlteredOrigin m =
  case hasComplexOrigin m of
    (False, _) -> m
    (True, _) -> case getResponse m "origin-s1" of
      Nothing -> m
      Just ocs -> case List.member ocs (originSkills m) of
        True -> m
        False -> killResponse m "origin-s1"

getLevel m =
  case getResponse m "basics-level" of
    Nothing -> 1
    Just x -> case (toInt x) of
      Ok l -> l
      Err _ -> 1

validateLevel m =
  case getResponse m "basics-level" of
    Nothing -> setResponse m "basics-level" "1"
    Just x -> case (toInt x) of
      Err _ -> setResponse m "basics-level" "1"
      Ok l -> if l < 1 then setResponse m "basics-level" "1" else
              if l > 10 then setResponse m "basics-level" "10" else m


fieldChanged field m =
  case field of
    "basics-origin" -> validateAlteredOrigin m
    "basics-level" -> validateLevel m
    _ -> m



basicsForm model = makeForm "The Basics" [
  DropdownField {name="Background", key="basics-bg", choices=(Dict.keys model.database.backgrounds)},
  DropdownField {name="Origin", key="basics-origin", choices=(Dict.keys model.database.origins)},
  NumberField {name="Level", key="basics-level"}]


getForms model =
   [basicsForm model] ++ (mayList (complexOriginForm model))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    HTTPLoadError e -> (httpError model, Cmd.none)
    BackgroundsLoaded bgs -> (unpackBackgrounds bgs model, getJsonFileCommand "data/origins.json" OriginsLoaded)
    OriginsLoaded ogs -> (unpackOrigins ogs model, Cmd.none)
    FormFieldUpdated k s -> (updateFieldResponse k s model, Cmd.none)
    _ -> (model, Cmd.none)
