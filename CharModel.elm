module CharModel exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (map, head, length, filter, sortBy)
import Html.App as Html
import Http exposing (getString, Error)
import Task exposing (perform)
import Result exposing (withDefault)
import ModelDB exposing (..)
import Dict exposing (Dict, insert)
import String exposing (toInt)
import FormsModel exposing (..)


init = ({character = blankCharacter,
         database = blankDatabase},
         getJsonFileCommand "data/backgrounds.json" BackgroundsLoaded)


{-| Turns a maybe value into a single element list for concatting. -}
mayList : Maybe x -> List x
mayList x = case x of
  Nothing -> []
  Just y -> [y]

{-| Reads the character's background skill list. -}
backgroundSkills : Model -> List String
backgroundSkills m = indirectLookup m "basics-bg" m.database.backgrounds
  .skillNames [] ["Missing background"]

{-| Reads the character's origin skill list. -}
originSkills : Model -> List String
originSkills m = indirectLookup m "basics-origin" m.database.origins
  .skillNames [] ["Missing origin"]

originComplications m = indirectLookup m "basics-origin" m.database.origins
  .complications [] ["Missing origin"]

-- Origins can be a bit of a nuisance because some of them have more skills and
-- complications than they're supposed to have, and the player is supposed to
-- choose which ones to go for. So we have to offer a choice if we have one of
-- these.

{-| Is an origin complex and if so, why? -}
complexOrigin : Origin -> (Bool, Bool)
complexOrigin x = ((length x.skillNames) > 2, (length x.complications) > 1)

{-| Is this model's origin conmplex and if so, why? -}
hasComplexOrigin : Model -> (Bool, Bool)
hasComplexOrigin m = indirectLookup m "basics-origin" m.database.origins
  complexOrigin (False, False) (False, False)

{-| Form for making the choices resulting from having a complex origin. -}
complexOriginForm : Model -> Maybe Form
complexOriginForm m = case (hasComplexOrigin m) of
  (False, False) -> Nothing
  (complexSkills, complexComplications) ->
    let
      skillPart = case complexSkills of
        True -> [DropdownField {name="First Skill:",seq=0,key="origin-s1",choices=([""] ++ originSkills m)},
                 DropdownField {name="Second Skill:",seq=1,key="origin-s2",choices=([""] ++ originSkills m)}]
        False -> []
      complicationPart = case complexComplications of
        True -> [DropdownField {name="Complication:",seq=2,key="origin-co",choices=([""] ++ originComplications m)}]
        False -> []
    in
      Just (Form "Origin" (skillPart ++ complicationPart))

{-| Gets the two skills our origin actually contributes, based on the list
and/or choices. -}
resolvedOriginSkills : Model -> List String
resolvedOriginSkills m = case (hasComplexOrigin m) of
  (False, _) -> originSkills m
  (True, _) -> mayList (Dict.get "origin-s1" m.character) ++
               mayList (Dict.get "origin-s2" m.character)

{-| Gets the list of character skills. -}
getSkills : Model -> List Skill
getSkills m = map (skillNameToSkill 0) (backgroundSkills m) -- Background
          ++ map (skillNameToSkill 1) (resolvedOriginSkills m) -- Origin

{-| Turns a skill name from the database into a skill storable on the character,
setting the source and name according to the parameters. -}
skillNameToSkill : Int -> String -> Skill
skillNameToSkill source name = { name = name, source = source }


killOutOfRange field list model =
  case getResponse model field of
    Nothing -> model
    Just val -> case List.member val list of
      True -> model
      False -> killResponse model field


validateAlteredOrigin m =
  case hasComplexOrigin m of
    (False, False) -> m
    _ -> (killOutOfRange "origin-s1" (originSkills m)
           (killOutOfRange "origin-s2" (originSkills m)
             (killOutOfRange "origin-co" (originComplications m) m)))

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



basicsForm model = Form "The Basics" [
  DropdownField {name="Background", key="basics-bg", seq=0, choices=(Dict.keys model.database.backgrounds)},
  DropdownField {name="Origin", key="basics-origin", seq=1, choices=(Dict.keys model.database.origins)},
  NumberField {name="Level", key="basics-level", seq=2}]

{-| Get the active forms. -}
getForms : Model -> List Form
getForms model =
   [basicsForm model] ++ (mayList (complexOriginForm model))

{-| Update a field on the character when a form value changes, and call validation. -}
updateFieldResponse : String -> String -> Model -> Model
updateFieldResponse key value model =
  fieldChanged key (setResponse model key value)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    FormFieldUpdated k s -> (updateFieldResponse k s model, Cmd.none)
    _ -> dbUpdate msg model
