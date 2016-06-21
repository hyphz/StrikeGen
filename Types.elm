
module Types exposing (..)
import Http exposing (getString, Error)
import Dict exposing (..)


type Msg =
    SkillChanged String Bool
  | BackgroundChanged String
  | OriginChanged String
  | BackgroundsLoaded String
  | OriginsLoaded String
  | HTTPLoadError Error
  | FormFieldUpdated String String

{-| The model in memory. Character holds the active character, Database holds the
current database read from the data files. -}
type alias Model =
    { character : Character,
      database : Database }

type alias Character =
    { origin : Origin,
      skills : List Skill,
      kits : List Kit,
      kitadvances : List KitAdvance,
      formkeys : List String,
      formresponses : Dict String String }

type alias Database =
    { backgrounds : Dict String Background,
      origins : Dict String Origin }


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
      complications : List String }

type alias Kit =
  { name : String,
    base : String,
    advances : List KitAdvance }

type alias KitAdvance =
  { name : String,
    desc : String }

type alias Form =
  { name : String,
    fields : Dict String Field }

type Field =
    DropdownField { name: String, key : String, choices: (List String) }
  | FreeformField { name: String, key : String }
  | NumberField { name: String, key: String }

nullBackground : Background
nullBackground = { name="<Not Selected>", skillNames=[], wealth=0, trick=""}
nullOrigin : Origin
nullOrigin = { name="<Not Selected>",skillNames=[],complications=[], wealth=0}
nullKit : Kit
nullKit = { name="<Not Selected>",base="",advances=[]}

-- Apparently we actually have to do this..
fieldKey x = case x of
  DropdownField df -> df.key
  FreeformField ff -> ff.key
  NumberField nf -> nf.key

makeForm : String -> List Field -> Form
makeForm name fields = {name = name,
  fields = Dict.fromList (List.map (\x -> (fieldKey x, x)) fields)}

blankResponses : Dict String String
blankResponses = Dict.fromList [("basics-level","1")]

blankCharacter = { skills = [],
          origin = nullOrigin,
          kits = [nullKit],
          kitadvances = [],
          formkeys = ["Sheep"],
          formresponses = blankResponses }

blankDatabase = { backgrounds = Dict.empty, origins = Dict.empty }
