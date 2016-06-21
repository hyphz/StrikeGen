module FormsModel exposing (..)
import Dict exposing (Dict)

-- Basic model for forms and entry.

-- Because character data is so variable, make it just a general dictionary.
type alias Character = Dict String String

-- A form for display.
type alias Form =
  { name : String,
    fields : List Field }

-- Field types.
type Field =
    DropdownField { name: String, key : String, choices: (List String) }
  | FreeformField { name: String, key : String  }
  | NumberField { name: String, key: String, min: Int, max: Int }

-- We have to do this because we're using tagged types instead of aliases.
fieldKey x = case x of
  DropdownField df -> df.key
  FreeformField ff -> ff.key
  NumberField nf -> nf.key
