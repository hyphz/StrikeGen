
module ModelDB exposing (..)
import Http exposing (getString, Error)
import Dict exposing (..)
import FormsModel exposing (..)
import Json.Decode exposing (Decoder, decodeString, (:=), object3, object4, object6,
  string, list, int, at, bool, oneOf, succeed)
import Result exposing (withDefault)
import Task exposing (perform)
import String exposing (toInt)
import Result exposing (Result)
import List


type Msg =
    SkillChanged String Bool
  | BackgroundChanged String
  | OriginChanged String
  | BackgroundsLoaded String
  | OriginsLoaded String
  | HTTPLoadError Error
  | FormFieldUpdated String String
  | FormAddClicked String
  | TextsLoaded String
  | DoSave

{-| The model in memory. Character holds the active character, Database holds the
current database read from the data files. -}
type alias Model =
    { character : Character,
      database : Database }

type alias Database =
    { backgrounds : Dict String Background,
      origins : Dict String Origin,
      texts : Dict String String }


type alias Skill =
    { name : String,
      source : Int }

type alias Background =
    { name : String,
      skillNames : List String,
      wealth : Int,
      trick : String }

type alias Origin =
    { name : String,
      skillNames : List String,
      wealth : Int,
      complications : List String,
      freeformSkill : Bool, -- Those pesky humans
      freeformComplication : Bool }

type alias Kit =
  { name : String,
    base : String,
    advances : List KitAdvance }

type alias KitAdvance =
  { name : String,
    desc : String }

type alias Power =
  { name : String,
    text : String,
    slot : Slot,
    freq : Freq,
    range : Int,
    area : Int,
    damage : Int,
    styl : PowerStyle }

type alias Class =
  { name : String,
    classPowerList : (Model -> List Power),
    classForms : (Model -> List Form),
    modifyBasicMeleeDamage : (Model -> Int),
    modifyBasicRangeDamage : (Model -> Int),
    modifyBasicRangeRange : (Model -> Int) }

type Slot = Role | Attack | Misc | Special | Reaction
type Freq = AtWill | Encounter | None
type PowerStyle = White | Red | Blue | Yellow | Green | Purple


setResponse model key value =
  let
    char = model.character
  in
   { model | character = Dict.insert key value char }

killResponse model key =
  let
    char = model.character
  in
   { model | character = Dict.remove key char }

getResponse model key =
  Dict.get key model.character

getResponseInt model key default =
  case (Dict.get key model.character) of
    Nothing -> default
    Just x -> case (toInt x) of
      Err _ -> default
      Ok i -> i

getLevel m =
  case getResponse m "basics-level" of
    Nothing -> 1
    Just x -> case (toInt x) of
      Ok l -> l
      Err _ -> 1

overtext model key default = Maybe.withDefault default (get key model.database.texts)



updateDatabase : (Database -> Database) -> Model -> Model
updateDatabase updater model = {model | database = (updater model.database) }

httpError : Model -> Model
httpError model =  model


originsDecoder : Decoder (List Origin)
originsDecoder = "origins" := (Json.Decode.list originDecoder)

originDecoder : Decoder Origin
originDecoder =
  Json.Decode.object6 Origin
    ("name" := string)
    ("skillNames" := Json.Decode.list string)
    ("wealth" := int)
    ("complications" := Json.Decode.list string)
    (Json.Decode.oneOf [("freeformSkill" := bool), succeed False])
    (Json.Decode.oneOf [("freeformComplication" := bool), succeed False])

backgroundsDecoder : Decoder (List Background)
backgroundsDecoder = "backgrounds" := (Json.Decode.list backgroundDecoder)

backgroundDecoder : Decoder Background
backgroundDecoder =
  object4 Background
    ("name" := string)
    ("skillNames" := Json.Decode.list string)
    ("wealth" := int)
    ("trick" := string)


{-| Returns the command to load a JSON data file. If it loads successfully, send the
specified message. If it doesn't, send HTTPLoadError.  -}
getJsonFileCommand : String -> (String -> Msg) -> Cmd Msg
getJsonFileCommand fileName signal =
   perform HTTPLoadError signal (getString fileName)

toDict : (a -> comparable) -> List a -> Dict comparable a
toDict keygetter list = Dict.fromList (List.map (\s -> (keygetter s, s)) list)

unpackBackgrounds : String -> Model -> Model
unpackBackgrounds s model = updateDatabase ( \d ->
 { d | backgrounds = toDict .name ([nullBackground] ++ (withDefault [] (decodeString backgroundsDecoder s)))}) model

unpackOrigins : String -> Model -> Model
unpackOrigins s model = updateDatabase ( \d ->
 { d | origins = toDict .name ([nullOrigin] ++ (withDefault [] (decodeString originsDecoder s)))}) model

nullBackground : Background
nullBackground = { name="<Not Selected>", skillNames=[], wealth=0, trick=""}
nullOrigin : Origin
nullOrigin = { name="<Not Selected>",skillNames=[],complications=[], wealth=0,
  freeformSkill = False, freeformComplication = False}
nullKit : Kit
nullKit = { name="<Not Selected>",base="",advances=[]}

blankCharacter : Dict String String
blankCharacter = Dict.fromList [("basics-level","1"),
                                ("basics-bg","<Not Selected>")]

blankDatabase = { backgrounds = Dict.empty, origins = Dict.empty, texts = Dict.empty }


splitTexts str =
  let
    paragraphs = String.split "@@" str
    brokenParas = List.map String.lines paragraphs
    extractParaKey s =
      let
        theTail = Maybe.withDefault ["Header without a body in texts??"] (List.tail s)
        theHead = Maybe.withDefault "BrokenHeader" (List.head s)
      in
        (theHead,String.join "\n" theTail)
    paraPairs = List.map extractParaKey brokenParas
  in
    Dict.fromList paraPairs

unpackTexts str model = updateDatabase ( \d -> {d | texts = splitTexts str} ) model

indirectLookup : Model -> String -> Dict String a -> (a -> b) -> b -> b -> b
indirectLookup model key db func default error =
  case Dict.get key model.character of
    Nothing -> default
    Just x -> case Dict.get x db of
      Nothing -> default
      Just o -> func o

dbUpdate : Msg -> Model -> (Model, Cmd Msg)
dbUpdate msg model = case msg of
    HTTPLoadError e -> (httpError model, Cmd.none)
    BackgroundsLoaded bgs -> (unpackBackgrounds bgs model, getJsonFileCommand "data/origins.json" OriginsLoaded)
    OriginsLoaded ogs -> (unpackOrigins ogs model, getJsonFileCommand "data/texts.md" TextsLoaded)
    TextsLoaded txs -> (unpackTexts txs model, Cmd.none)
    _ -> (model, Cmd.none)
