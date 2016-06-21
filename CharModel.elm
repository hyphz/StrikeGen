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
  (complexSkills, complexComplications, freeformSkill, _) ->
    let
      skillPart = case complexSkills of
        True -> [DropdownField {name="First Skill:",key="origin-s1",choices=([""] ++ originSkills m)},
                 DropdownField {name="Second Skill:",key="origin-s2",choices=([""] ++ originSkills m)}]
        False -> []
      complicationPart = case complexComplications of
        True -> [DropdownField {name="Complication:",key="origin-co",choices=([""] ++ originComplications m)}]
        False -> []
      freeformSkillPart = case freeformSkill of
        True -> [FreeformField {name="Custom skill:",key="origin-cs"}]
        False -> []
      originName = case getResponse m "basics-origin" of
        Just x -> x
        Nothing -> "(BUG) Origin complex but missing"
    in
      Just (Form originName (skillPart ++ complicationPart ++ freeformSkillPart))

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

{-| Turns a skill name from the database into a skill storable on the character,
setting the source and name according to the parameters. -}
skillNameToSkill : Int -> String -> Skill
skillNameToSkill source name = { name = name, source = source }

customBackgroundFields : Model -> List Field
customBackgroundFields m = [
   FreeformField {name="Skill you need the most?", key="bg-custom-s1"},
   FreeformField {name="Supporting skill or knowledge?", key="bg-custom-s2"},
   FreeformField {name="Social/business skill?", key="bg-custom-s3"},
   DropdownField {name="What helps when times are tough?", key="bg-custom-wos1", choices=["","Wealth","People"]}]
   ++ mayList (
        case getResponse m "bg-custom-wos1" of
          Just "People" -> Just <| FreeformField {name="Who?", key="bg-custom-wos1s"}
          _ -> Nothing
        )
   ++ [DropdownField {name="How you get stuff?", key="bg-custom-wos2", choices=["","Money","Skill"]}]
   ++ mayList (
        case getResponse m "bg-custom-wos2" of
          Just "Skill" -> Just <| FreeformField {name="What skill?", key="bg-custom-wos2s"}
          _ -> Nothing
        )

resolvedBackgroundSkills m = case getResponse m "basics-bg" of
  Nothing -> []
  Just "<Custom>" -> (mayList <| getResponse m "bg-custom-s1") ++
                     (mayList <| getResponse m "bg-custom-s2") ++
                     (mayList <| getResponse m "bg-custom-s3") ++
                      if (getResponse m "bg-custom-wos1" == Just "People") then mayList <| getResponse m "bg-custom-wos1s" else [] ++
                      if (getResponse m "bg-custom-wos2" == Just "Skill") then mayList <| getResponse m "bg-custom-wos2s" else []
  Just _ -> backgroundSkills m

customBackgroundForm : Model -> Maybe Form
customBackgroundForm m =
  case getResponse m "basics-bg" of
    Nothing -> Nothing
    Just "<Custom>" -> Just (Form "Custom background" (customBackgroundFields m))
    Just _ -> Nothing

{-| Quick function for removing a field value that's out of range, if it
exists. -}
killOutOfRange field list model =
  case getResponse model field of
    Nothing -> model
    Just val -> case List.member val list of
      True -> model
      False -> killResponse model field


{-| When origin is changed, if it's changed to a new complex origin,
remove any skill choices made for a previous complex origin that aren't
relevant to the new one. -}
validateAlteredOrigin m =
  case hasComplexOrigin m of
    (False, False, False, False) -> m
    _ -> (killOutOfRange "origin-s1" (originSkills m)
           (killOutOfRange "origin-s2" (originSkills m)
             (killOutOfRange "origin-co" (originComplications m) m)))

getLevel m =
  case getResponse m "basics-level" of
    Nothing -> 1
    Just x -> case (toInt x) of
      Ok l -> l
      Err _ -> 1

{-| Validate the level setting. This shouldn't ever become invalid, but
better safe than sorry, I guess.. -}
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
  DropdownField {name="Background", key="basics-bg", choices=(["<Custom>"] ++ (Dict.keys model.database.backgrounds))},
  DropdownField {name="Origin", key="basics-origin", choices=(Dict.keys model.database.origins)},
  NumberField {name="Level", key="basics-level", min=1, max=10}]

{-| Get the active forms. -}
getForms : Model -> List Form
getForms model =
   [basicsForm model] ++
   (mayList (complexOriginForm model)) ++
   (mayList (customBackgroundForm model))


{-| Update a field on the character when a form value changes, and call validation. -}
updateFieldResponse : String -> String -> Model -> Model
updateFieldResponse key value model =
  fieldChanged key (setResponse model key value)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    FormFieldUpdated k s -> (updateFieldResponse k s model, Cmd.none)
    _ -> dbUpdate msg model
