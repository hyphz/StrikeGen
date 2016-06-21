
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
  | FreeformFieldUpdated String String

{-| The model in memory. Character holds the active character, Database holds the
current database read from the data files. -}
type alias Model =
    { character : Character,
      database : Database }

type alias Character =
    { background : Background,
      origin : Origin,
      skills : List Skill,
      kits : List Kit,
      kitadvances : List KitAdvance,
      formkeys : List String,
      formresponses : Dict String String }

type alias Database =
    { backgrounds : List Background,
      origins : List Origin,
      forms : Dict String Form }


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
    DropdownField { name: String, key : String, choices: (List String), value: String }
  | FreeformField { name: String, key : String, value: String }

nullBackground : Background
nullBackground = { name="<Not Selected>", skillNames=[], wealth=0, trick=""}
nullOrigin : Origin
nullOrigin = { name="<Not Selected>",skillNames=[],complications=[], wealth=0}
nullKit : Kit
nullKit = { name="<Not Selected>",base="",advances=[]}




testForm : Dict String Form
testForm = singleton "Sheep" {name="Sheep",
  fields=(singleton "Sheep" (FreeformField { name="Cow", key="Sheep", value="" }))}


blankCharacter = { skills = [],
          background = nullBackground,
          origin = nullOrigin,
          kits = [nullKit],
          kitadvances = [],
          formkeys = ["Sheep"],
          formresponses = Dict.empty }

blankDatabase = { backgrounds = [nullBackground], origins = [nullOrigin], forms = testForm }
