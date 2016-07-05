
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
import Ports exposing (dbLoaded)

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
  | FileCommand String
  | LoadJson String
  | FieldDeleteClicked String



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
    modifyBasicMelee : Maybe (Model -> Power -> Power),
    modifyBasicRange : Maybe (Model -> Power -> Power),
    modifyCharge : Maybe (Model -> Power -> Power),
    modifyRally : Maybe (Model -> Power -> Power) }

type alias Role =
  { name : String,
    rolePowerList : (Model -> List Power),
    roleForms : (Model -> List Form)}

type Slot = RoleSlot | Attack | Misc | Special | Reaction
type Freq = AtWill | Encounter | None
type PowerStyle = White | Red | Blue | Yellow | Green | Purple

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

blankDatabase : Database
blankDatabase = { backgrounds = Dict.empty, origins = Dict.empty, texts = Dict.empty }



{-| Turns a maybe value into a single element list for concatting. -}
mayList : Maybe x -> List x
mayList x = case x of
  Nothing -> []
  Just y -> [y]

{-| Updates the character store with a form response. -}
setResponse : Model -> String -> String -> Model
setResponse model key value =
  let
    char = model.character
  in
   { model | character = Dict.insert key value char }

{-| Removes a form response from the character store. -}
killResponse : Model -> String -> Model
killResponse model key =
  let
    char = model.character
  in
   { model | character = Dict.remove key char }

{-| Gets a form response from the character store. -}
getResponse : Model -> String -> Maybe String
getResponse model key =
  Dict.get key model.character

{-| Moves a form response from one key to another. -}
moveResponse : Model -> String -> String -> Model
moveResponse m src dest =
  case (getResponse m src) of
    Nothing -> killResponse m dest -- Move the "nothing" by deleting the destination
    Just r -> setResponse (killResponse m src) dest r


{-| Gets a form response from the character store, and runs a function on it
if it exists; otherwise, returns the default value. -}
ifResponse : Model -> String -> a -> (String -> a) -> a
ifResponse model key default func =
  case (getResponse model key) of
    Just x -> func x
    Nothing -> default

{-| Gets a form response from the character store and casts it to Int. Returns
the default if it is missing or not an int. -}
getResponseInt : Model -> String -> Int -> Int
getResponseInt model key default =
  case (Dict.get key model.character) of
    Nothing -> default
    Just x -> case (toInt x) of
      Err _ -> default
      Ok i -> i

{-| Gets the character's level. -}
getLevel : Model -> Int
getLevel m =
  getResponseInt m "basics-level" 1


{-| Gets a value from the character store, looks it up in another dictionary, and
then runs a function on it if it's found. If it's not in the store, returns default.
If the value from the store isn't in the dictionary, returns error. -}
indirectLookup : Model -> String -> Dict String a -> (a -> b) -> b -> b -> b
indirectLookup model key db func default error =
  ifResponse model key default (\x ->
    case Dict.get x db of
      Nothing -> error
      Just o -> func o)


{-| Looks up the given key in the text database and returns the text if found,
otherwise returns the default. -}
overtext : Model -> String -> String
overtext model key = Maybe.withDefault ("(Text not available " ++ key ++")") (get key model.database.texts)


{-| Updates the database part of the model.-}
updateDatabase : (Database -> Database) -> Model -> Model
updateDatabase updater model = {model | database = (updater model.database) }

{-| What to do with the model if a data load fails. -}
httpError : Model -> Model
httpError model =  model


originsDecoder : Decoder (List Origin)
originsDecoder = "origins" := (Json.Decode.list
    (Json.Decode.object6 Origin
      ("name" := string)
      ("skillNames" := Json.Decode.list string)
      ("wealth" := int)
      ("complications" := Json.Decode.list string)
      (Json.Decode.oneOf [("freeformSkill" := bool), succeed False])
      (Json.Decode.oneOf [("freeformComplication" := bool), succeed False])))

backgroundsDecoder : Decoder (List Background)
backgroundsDecoder = "backgrounds" := (Json.Decode.list
  (object4 Background
    ("name" := string)
    ("skillNames" := Json.Decode.list string)
    ("wealth" := int)
    ("trick" := string)))



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


{-| Parses the text file blob into the text database. -}
splitTexts : String -> Dict String String
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

unpackTexts : String -> Model -> Model
unpackTexts str model = updateDatabase ( \d -> {d | texts = splitTexts str} ) model

dbUpdate : Msg -> Model -> (Model, Cmd Msg)
dbUpdate msg model = case msg of
    HTTPLoadError e -> (httpError model, Ports.alert "Database load error! Check internet or local data/ path.")
    BackgroundsLoaded bgs -> (unpackBackgrounds bgs model, getJsonFileCommand "data/origins.json" OriginsLoaded)
    OriginsLoaded ogs -> (unpackOrigins ogs model, getJsonFileCommand "data/texts.md" TextsLoaded)
    TextsLoaded txs -> (unpackTexts txs model, Ports.dbLoaded 0)
    _ -> (model, Cmd.none)
