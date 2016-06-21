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
import Dict exposing (insert)

init = ({character = blankCharacter,
         database = blankDatabase},
         getJsonFileCommand "data/backgrounds.json" BackgroundsLoaded)

{-| Returns the command to load a JSON data file. If it loads successfully, send the
specified message. If it doesn't, send HTTPLoadError.  -}
getJsonFileCommand : String -> (String -> Msg) -> Cmd Msg
getJsonFileCommand fileName signal =
   perform HTTPLoadError signal (getString fileName)


{-| Gets the one member of a list that satisfies a condition, or throws an error
if there's none or more than one. -}
theOne : (a -> Bool) -> List a -> (Result String a)
theOne f l =
    let filteredList = (filter f l) in
    case (length filteredList) of
        2 -> Err "Multiple entries matched theOne"
        _ -> maybeToResult "No entry matched theOne" (head filteredList)

{-| Model update function that searches the provided list for a item with a matching
name, and if it is found, calls another model update function with the model and the
found entry; otherwise, leaves the model untouched. -}
fetchAndUpdate : List a -> String -> (a -> String) -> (a -> Model -> Model) -> Model -> Model
fetchAndUpdate theList target nameExtractor updater model =
    case (theOne (\x -> (nameExtractor x) == target) theList) of
        Ok x -> updater x model
        Err _ -> model



{-| Turns a Maybe value into a Result, using the provided string as the error message
if the Maybe is absent. -}
maybeToResult : String -> (Maybe a) -> (Result String a)
maybeToResult err may = case may of
    Just x -> Ok x
    Nothing -> Err err

{-| Turns a skill name from the database into a skill storable on the character,
setting the source and name according to the parameters. -}
skillNameToSkill : Int -> String -> Skill
skillNameToSkill source name = { name = name, source = source }


{-| Adds all skill names in the string list to the given skill list, giving them
the specific source tag. -}
addSkillsWithSource : Int -> List String -> List Skill -> List Skill
addSkillsWithSource source newskills skills =
   skills ++ map (skillNameToSkill source) newskills

{-| Deletes all skill names in the given skill list that have a specific source
tag. -}
wipeSkillsWithSource : Int -> List Skill -> List Skill
wipeSkillsWithSource target = filter (\s -> s.source /= target)


{-| Replaces all skills in the given skill list which have the given source tag
with newly generated skills with the same source tag generated from the given
list of strings. -}
switchSkillsWithSource : Int -> List String -> List Skill -> List Skill
switchSkillsWithSource source newskills oldskills =
    addSkillsWithSource source (newskills) (wipeSkillsWithSource source oldskills)

updateCharacter : (a -> Character -> Character) -> a -> Model -> Model
updateCharacter updater updata model = {model | character = (updater updata model.character) }

updateDatabase : (Database -> Database) -> Model -> Model
updateDatabase updater model = {model | database = (updater model.database) }



updateForNewBackground : Background -> Character -> Character
updateForNewBackground newbg model =
    { model | background = newbg, skills = (switchSkillsWithSource 0 newbg.skillNames model.skills) }

applyBackgroundChange : String -> Model -> Model
applyBackgroundChange newname model =
    fetchAndUpdate model.database.backgrounds newname .name (updateCharacter updateForNewBackground) model

updateForNewOrigin newo model =
    { model  | origin = newo, skills = (switchSkillsWithSource 1 newo.skillNames model.skills) }

applyOriginChange newname model =
    fetchAndUpdate model.database.origins newname .name (updateCharacter updateForNewOrigin) model

httpError : Model -> Model
httpError model = (updateDatabase (\db -> { db | backgrounds = [nullBackground] } )) model


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

unpackBackgrounds : String -> Model -> Model
unpackBackgrounds s model = updateDatabase ( \d ->
       { d | backgrounds = [nullBackground] ++ (withDefault [] (decodeString backgroundsDecoder s))}) model

unpackOrigins : String -> Model -> Model
unpackOrigins s model = updateDatabase ( \d ->
       { d | origins = [nullOrigin] ++ (withDefault [] (decodeString originsDecoder s))}) model

updateFieldResponse key value model =
  updateCharacter (\junk char -> { char | formresponses = Dict.insert key value char.formresponses }) 0 model



update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    BackgroundChanged x -> (applyBackgroundChange x model, Cmd.none)
    OriginChanged x -> (applyOriginChange x model, Cmd.none)
    HTTPLoadError e -> (httpError model, Cmd.none)
    BackgroundsLoaded bgs -> (unpackBackgrounds bgs model, getJsonFileCommand "data/origins.json" OriginsLoaded)
    OriginsLoaded ogs -> (unpackOrigins ogs model, Cmd.none)
    FreeformFieldUpdated k s -> (updateFieldResponse k s model, Cmd.none)
    _ -> (model, Cmd.none)
