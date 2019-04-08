module CharModel exposing (OriginComplexity, backgroundSkills, basicsForm, complexOrigin, complexOriginForm, customBackgroundForm, customOriginForm, encodeChar, extraCompsForm, featsForm, fieldChanged, getComplications, getExtraComplications, getForms, getLearnedSkills, getLearnedTricks, getSkills, getTricks, getWealth, handleFileCommand, hasComplexOrigin, importChar, init, learnedSkillsForm, learnedTricksForm, levelForm, originComplications, originSkills, repForm, repLine, resolvedBackgroundSkills, resolvedOriginComplications, resolvedOriginSkills, setOriginSkills, simpleOriginComplexity, skillNameToSkill, update, updateExtendForm, updateFieldResponse, validateAlteredOrigin, validateLevel)

import Dict exposing (Dict, insert)
import ExtenderForms exposing (extendForm, extenderForm, getExtendFormEntries, updateDeleteField)
import FormsModel exposing (..)
import Json.Decode
import Json.Encode exposing (encode)
import Kits exposing (kitsForm, validateKitProgression)
import List exposing (filter, head, length, map, sortBy)
import ModelDB exposing (..)
import Ports exposing (download, saveURL)
import Result exposing (withDefault)
import Roll20 exposing (powerMacros)
import String exposing (concat, toInt)
import TacticalModel exposing (availableFeats, classes, featChoices, roles, tacticalForms)


{-| ELM Architecture Initialization Function.
-}
init : Int -> ( Model, Cmd Msg )
init _ =
    ( { character = blankCharacter
      , database = blankDatabase
      }
    , getJsonFileCommand "data/backgrounds.json" BackgroundsLoaded
    )


{-| Reads the character's background skill list.
-}
backgroundSkills : Model -> List String
backgroundSkills m =
    indirectLookup m
        "basics-bg"
        m.database.backgrounds
        .skillNames
        []
        [ "Missing background" ]


{-| Reads the character's origin skill list.
-}
originSkills : Model -> List String
originSkills m =
    indirectLookup m
        "basics-origin"
        m.database.origins
        .skillNames
        []
        [ "Missing origin" ]


{-| Calculates the character's wealth from their origin and background.
-}
getWealth : Model -> Int
getWealth m =
    let
        backgroundWealth =
            case getResponse m "basics-bg" of
                {- If custom background, calculate wealth from answers to standard questions. -}
                Just "<Custom>" ->
                    (if getResponse m "bg-custom-wos1" == Just "Wealth" then 1 else 0)
                        + (if getResponse m "bg-custom-wos2" == Just "Money" then 1 else 0)
                {- Else, just get wealth from background. -}
                _ ->
                    indirectLookup m "basics-bg" m.database.backgrounds .wealth 0 0

        originWealth =
            case getResponse m "basics-origin" of
                {-If custom origin, calculate wealth from answer to question. -}
                Just "<Custom>" ->
                    if getResponse m "origin-custom-wos1" == Just "Wealth" then 1 else 0
                {- Else. just get wealth from background. -}
                _ ->
                    indirectLookup m "basics-origin" m.database.origins .wealth 0 0
    in
        backgroundWealth + originWealth


{-| Reads the character's origin complication list.
-}
originComplications : Model -> List String
originComplications m =
    indirectLookup m
        "basics-origin"
        m.database.origins
        .complications
        []
        [ "Missing origin" ]



{- Origins can be a bit of a nuisance because some of them have more skills and
  complications than they're supposed to have, and the player is supposed to
  choose which ones to go for. So we have to offer a choice if we have one of
  these. -}


type alias OriginComplexity =
    { complexSkills : Bool
    , complexComplication : Bool
    , freeformSkill : Bool
    , freeformComplication : Bool
    }


simpleOriginComplexity : OriginComplexity
simpleOriginComplexity =
    { complexSkills = False
    , complexComplication = False
    , freeformSkill = False
    , freeformComplication = False
    }


{-| Is an origin complex and if so, why?
-}
complexOrigin : Origin -> OriginComplexity
complexOrigin x =
    { complexSkills = length x.skillNames > 2  {- Because there's a choice of skills? -}
    , complexComplication = length x.complications > 1 {- Because there's a choice of complications? -}
    , freeformSkill = x.freeformSkill {- Because there's a freeform skill? -}
    , freeformComplication = x.freeformComplication {- Because there's a freeform complication? -}
    }


{-| Is this model's origin complex and if so, why?
-}
hasComplexOrigin : Model -> OriginComplexity
hasComplexOrigin m =
    indirectLookup m
        "basics-origin"
        m.database.origins
        complexOrigin
        simpleOriginComplexity
        simpleOriginComplexity


{-| Form for making the choices resulting from having a complex origin.
-}
complexOriginForm : Model -> Maybe Form
complexOriginForm m =
    let
        formParts { complexSkills, complexComplication, freeformSkill, freeformComplication } =
            let
                skillPart =
                    {- If complex skills, add choice dropdowns for skills. -}
                    case complexSkills of
                        True ->
                            [ DropdownField { name = "First Skill:", del = False, key = "origin-s1", choices = [ "" ] ++ originSkills m }
                            , DropdownField { name = "Second Skill:", del = False, key = "origin-s2", choices = [ "" ] ++ originSkills m }
                            ]

                        False ->
                            []

                complicationPart =
                    {- If complex complication, add choice dropdown for complication. -}
                    case complexComplication of
                        True ->
                            [ DropdownField { name = "Complication:", del = False, key = "origin-co", choices = [ "" ] ++ originComplications m } ]

                        False ->
                            []

                freeformSkillPart =
                    {- If freeform skill, add freeform skill field. -}
                    case freeformSkill of
                        True ->
                            [ FreeformField { name = "Custom skill:", del = False, key = "origin-cs" } ]

                        False ->
                            []

                freeformComplicationPart =
                    {- If freeform complication, add freeform complication field. -}
                    case freeformComplication of
                        True ->
                            [ FreeformField { name = "Complication:", del = False, key = "origin-cco" } ]

                        False ->
                            []

                originName =
                    {- Get origin name. It should be defined since this function shouldn't be running
                       if the origin isn't complex, and if we know it's complex we should know the name. -}
                    case getResponse m "basics-origin" of
                        Just x ->
                            x

                        Nothing ->
                            "(BUG) Origin complex but missing"
            in
            Just (Form False originName (skillPart ++ complicationPart ++ freeformSkillPart ++ freeformComplicationPart))
    in
    formParts (hasComplexOrigin m)


{-| Calculate the complications arising from the origin, allowing for freeforms and choices.
-}
resolvedOriginComplications : Model -> List String
resolvedOriginComplications m =
    case getResponse m "basics-origin" of
        {- If it's a custom origin, get the custom freeform complication. -}
        Just "<Custom>" ->
            mayList (getResponse m "origin-custom-co")

        _ ->
            {- If complex complication, get the chosen complication; else, get the origin's
               single complication.-}
            (if .complexComplication (hasComplexOrigin m) then
                mayList (getResponse m "origin-co")
             else
                indirectLookup m "basics-origin" m.database.origins .complications [] []
            )
                {- If freeform complication, include it. -}
                ++ (if .freeformComplication (hasComplexOrigin m) then
                        mayList (getResponse m "origin-cco")
                    else
                        []
                   )


{-| Form for a custom origin.
-}
customOriginForm : Model -> List Form
customOriginForm m =
    case getResponse m "basics-origin" of
        Just "<Custom>" ->
            [ Form False
                "Custom Origin"
                ([ FreeformField { name = "Skill:", del = False, key = "origin-custom-s1" }
                 , FreeformField { name = "Complication:", del = False, key = "origin-custom-co" }
                 , DropdownField { name = "Bonus:", del = False, key = "origin-custom-wos1", choices = [ "", "Skill", "Wealth" ] }
                 ]
                    ++ (case getResponse m "origin-custom-wos1" of
                            Just "Skill" ->
                                [ FreeformField { name = "Skill:", del = False, key = "origin-custom-s2" } ]

                            _ ->
                                []
                       )
                )
            ]

        _ ->
            []


{-| Gets the two skills our origin actually contributes, based on the list
and/or choices.
-}
setOriginSkills : Model -> List String
setOriginSkills m =
    let
        hco =
            hasComplexOrigin m
    in
    {- If no skill choices and no freeform skill, just use skills from the origin template. -}
    if not hco.complexSkills && not hco.freeformSkill then
        originSkills m

    {- If complex skills and no freeform skill, get the two skill choices. -}
    else if hco.complexSkills && not hco.freeformSkill then
        mayList (Dict.get "origin-s1" m.character)
            ++ mayList (Dict.get "origin-s2" m.character)

    {- If freeform skill and no complex skill, get the template fixed skills and the freeform one. -}
    else if not hco.complexSkills && hco.freeformSkill then
        originSkills m ++ mayList (Dict.get "origin-cs" m.character)

    {- Else, get the chosen skill and the freeform skill. -}
    else
        mayList (Dict.get "origin-s1" m.character)
            ++ mayList (Dict.get "origin-cs" m.character)


{-| Gets the list of character skills.
-}
getSkills : Model -> List Sourced
getSkills m =
    map (skillNameToSkill 0) (resolvedBackgroundSkills m)
        -- Background
        ++ map (skillNameToSkill 1) (resolvedOriginSkills m)
        -- Origin
        ++ mayList (Maybe.map (skillNameToSkill 2) (getResponse m "basics-skill"))
        ++ map (skillNameToSkill 2) (getLearnedSkills m)


{-| Turns a skill name from the database into a skill storable on the character,
setting the source and name according to the parameters. This is actually used for
things other than skills because pairing a name and a source turns out to be
really useful, thus the "Sourced" type.
-}
skillNameToSkill : Int -> String -> Sourced
skillNameToSkill source name =
    { name = name, source = source }


{-| Form giving the standard prompts from the book for selecting skills for a custom
background.
-}
customBackgroundForm : Model -> Maybe Form
customBackgroundForm m =
    case getResponse m "basics-bg" of
        Nothing ->
            Nothing

        Just "<Custom>" ->
            Just
                (Form False
                    "Custom background"
                    ([ FreeformField { name = "Skill you need the most?", key = "bg-custom-s1", del = False }
                     , FreeformField { name = "Supporting skill or knowledge?", key = "bg-custom-s2", del = False }
                     , FreeformField { name = "Social/business skill?", key = "bg-custom-s3", del = False }
                     , FreeformField { name = "Other background skill?", key = "bg-custom-s4", del = False }
                     , DropdownField { name = "What helps when times are tough?", del = False, key = "bg-custom-wos1", choices = [ "", "Wealth", "People" ] }
                     ]
                        ++ mayList
                            (case getResponse m "bg-custom-wos1" of
                                Just "People" ->
                                    Just <| FreeformField { name = "Who?", del = False, key = "bg-custom-wos1s" }

                                _ ->
                                    Nothing
                            )
                        ++ [ DropdownField { name = "How do you get stuff?", del = False, key = "bg-custom-wos2", choices = [ "", "Money", "Skill" ] } ]
                        ++ mayList
                            (case getResponse m "bg-custom-wos2" of
                                Just "Skill" ->
                                    Just <| FreeformField { name = "What skill?", del = False, key = "bg-custom-wos2s" }

                                _ ->
                                    Nothing
                            )
                        ++ [ FreeformField { name = "Trick?", key = "bg-custom-t", del = False } ]
                    )
                )

        Just _ ->
            Nothing


{-| Returns the skill choices for the background, allowing for it being custom.
-}
resolvedBackgroundSkills : Model -> List String
resolvedBackgroundSkills m =
    case getResponse m "basics-bg" of
        Nothing ->
            []

        Just "<Custom>" ->
            (mayList <| getResponse m "bg-custom-s1")
                ++ (mayList <| getResponse m "bg-custom-s2")
                ++ (mayList <| getResponse m "bg-custom-s3")
                ++ (mayList <| getResponse m "bg-custom-s4")
                ++ (if getResponse m "bg-custom-wos1" == Just "People" then
                        mayList <| getResponse m "bg-custom-wos1s"

                    else
                        []
                   )
                ++ (if getResponse m "bg-custom-wos2" == Just "Skill" then
                        mayList <| getResponse m "bg-custom-wos2s"

                    else
                        []
                   )

        Just _ ->
            backgroundSkills m


{-| Returns the skill choices for the origin, allowing for it being custom.
-}
resolvedOriginSkills : Model -> List String
resolvedOriginSkills m =
    case getResponse m "basics-origin" of
        Nothing ->
            []

        Just "<Custom>" ->
            (mayList <| getResponse m "origin-custom-s1")
                ++ (if getResponse m "origin-custom-wos1" == Just "Skill" then
                        mayList <| getResponse m "origin-custom-s2"

                    else
                        []
                   )

        Just _ ->
            setOriginSkills m


{-| When origin is changed, if it's changed to a new complex origin,
remove any skill choices made for a previous complex origin that aren't
relevant to the new one.
-}
validateAlteredOrigin : Model -> Model
validateAlteredOrigin m =
    if hasComplexOrigin m == simpleOriginComplexity then
        m

    else
        killOutOfRange "origin-s1"
            (originSkills m)
            (killOutOfRange "origin-s2"
                (originSkills m)
                (killOutOfRange "origin-co" (originComplications m) m)
            )


{-| Validate the level setting. This shouldn't ever become invalid, but
better safe than sorry, I guess..
-}
validateLevel : Model -> Model
validateLevel m =
    case getResponse m "basics-level" of
        Nothing ->
            setResponse m "basics-level" "1"

        Just x ->
            case toInt x of
                Nothing ->
                    setResponse m "basics-level" "1"

                Just l ->
                    if l < 1 then
                        setResponse m "basics-level" "1"

                    else if l > 10 then
                        setResponse m "basics-level" "10"

                    else
                        m


{-| Performs validation when a field changes.
-}
fieldChanged : String -> Model -> Model
fieldChanged field m =
    case field of
        "basics-origin" ->
            validateAlteredOrigin m

        "basics-level" ->
            validateLevel m

        "kit-start" ->
            validateKitProgression m

        "kit-1" ->
            validateKitProgression m

        "kit-2" ->
            validateKitProgression m

        "kit-3" ->
            validateKitProgression m

        "kit-4" ->
            validateKitProgression m

        "kit-5" ->
            validateKitProgression m

        _ ->
            m


{-| Definitions of the three extendable forms.
-}
learnedSkillsForm : Model -> Form
learnedSkillsForm m =
    extenderForm m "Learned Skills" "Skill:" "ls"


learnedTricksForm : Model -> Form
learnedTricksForm m =
    extenderForm m "Learned Tricks" "Trick:" "lt"


extraCompsForm : Model -> Form
extraCompsForm m =
    extenderForm m "Extra Complications" "Complication:" "lc"


{-| Get the full list of tricks.
-}
getTricks : Model -> List Sourced
getTricks m =
    let
        bgTrick =
            if getResponse m "basics-bg" == Just "<Custom>" then
                case getResponse m "bg-custom-t" of
                    Nothing ->
                        []

                    Just t ->
                        [ t ]

            else
                case indirectLookup m "basics-bg" m.database.backgrounds .trick "" "" of
                    "" ->
                        []

                    x ->
                        [ x ]

        customTrick =
            mayList (getResponse m "basics-trick")

        levelTricks =
            (if getLevel m >= 2 then
                mayList (getResponse m "adv-l2trick")

             else
                []
            )
                ++ (if getLevel m >= 6 then
                        mayList (getResponse m "adv-l6trick")

                    else
                        []
                   )
                ++ (if getLevel m >= 10 then
                        mayList (getResponse m "adv-l10trick")

                    else
                        []
                   )

        organicTricks =
            case getResponse m "basics-ticks" of
                Just "Yes" ->
                    getLearnedTricks m

                _ ->
                    []
    in
    map (skillNameToSkill 0) bgTrick
        ++ map (skillNameToSkill 2) customTrick
        ++ map (skillNameToSkill 2) levelTricks
        ++ map (skillNameToSkill 2) organicTricks


{-| Get the full list of complications.
-}
getComplications : Model -> List Sourced
getComplications m =
    let
        innerOriginComplications =
            resolvedOriginComplications m

        customComplication =
            mayList (getResponse m "basics-comp")

        levelComplications =
            (if getLevel m >= 4 then
                mayList (getResponse m "adv-l4comp")

             else
                []
            )
                ++ (if getLevel m >= 8 then
                        mayList (getResponse m "adv-l8comp")

                    else
                        []
                   )

        organicComplications =
            case getResponse m "basics-ticks" of
                Just "Yes" ->
                    getExtraComplications m

                _ ->
                    []
    in
    map (skillNameToSkill 1) innerOriginComplications
        ++ map (skillNameToSkill 2) (customComplication ++ levelComplications ++ organicComplications)


{-| Form with the most basic common parts of a character.
-}
basicsForm : Model -> Form
basicsForm model =
    Form False
        "The Basics"
        [ FreeformField { name = "Name:", del = False, key = "basics-name" }
        , DropdownField { name = "Background:", del = False, key = "basics-bg", choices = [ "<Custom>" ] ++ Dict.keys model.database.backgrounds }
        , DropdownField { name = "Origin:", del = False, key = "basics-origin", choices = [ "<Custom>" ] ++ Dict.keys model.database.origins }
        , NumberField { name = "Level:", del = False, key = "basics-level", min = 1, max = 10 }
        , DropdownField { name = "Organic:", del = False, key = "basics-ticks", choices = [ "", "No", "Yes" ] }
        , DropdownField { name = "Class:", del = False, key = "basics-class", choices = [ "" ] ++ Dict.keys classes }
        , DropdownField { name = "Role:", del = False, key = "basics-role", choices = [ "" ] ++ Dict.keys roles }
        , FreeformField { name = "Custom Skill:", del = False, key = "basics-skill" }
        , FreeformField { name = "Custom Trick:", del = False, key = "basics-trick" }
        , FreeformField { name = "Complication:", del = False, key = "basics-comp" }
        ]


{-| Form holding the values set as a character advances in level.
-}
levelForm : Model -> Form
levelForm model =
    Form False
        "Advancement"
        ((if getLevel model >= 2 then
            [ FreeformField { name = "Fallback", del = False, key = "adv-fallback" }
            , FreeformField { name = "Trick", del = False, key = "adv-l2trick" }
            ]

          else
            []
         )
            ++ (if getLevel model >= 4 then
                    [ FreeformField { name = "Complication", del = False, key = "adv-l4comp" } ]

                else
                    []
               )
            ++ (if getLevel model >= 6 then
                    [ FreeformField { name = "Trick", del = False, key = "adv-l6trick" } ]

                else
                    []
               )
            ++ (if getLevel model >= 8 then
                    [ FreeformField { name = "Complication", del = False, key = "adv-l8comp" } ]

                else
                    []
               )
            ++ (if getLevel model >= 10 then
                    [ FreeformField { name = "Trick", del = False, key = "adv-l10trick" } ]

                else
                    []
               )
        )


{-| Form for feats.
-}
featsForm : Model -> Form
featsForm m =
    Form False
        "Feats"
        ([ DropdownField { name = "Feat:", del = False, key = "feat-1", choices = availableFeats m } ]
            ++ (if getLevel m >= 3 then
                    [ DropdownField { name = "Feat:", del = False, key = "feat-2", choices = availableFeats m } ]

                else
                    []
               )
            ++ (if getLevel m >= 5 then
                    [ DropdownField { name = "Feat:", del = False, key = "feat-3", choices = availableFeats m } ]

                else
                    []
               )
            ++ (if getLevel m >= 7 then
                    [ DropdownField { name = "Feat:", del = False, key = "feat-4", choices = availableFeats m } ]

                else
                    []
               )
            ++ (if getLevel m >= 9 then
                    [ DropdownField { name = "Feat:", del = False, key = "feat-5", choices = availableFeats m } ]

                else
                    []
               )
            ++ featChoices m
        )


repLine : Model -> Int -> List Field
repLine m ind =
    [ FreeformField { name = "Reputation:", del = False, key = "rep-" ++ String.fromInt ind }
    , DropdownField { name = "Type:", del = False, key = "rept-" ++ String.fromInt ind, choices = [ "", "Admired", "Famous", "Feared" ] }
    ]


repForm : Model -> Form
repForm m =
    Form False
        "Reputation"
        ((if getLevel m >= 2 then
            repLine m 1

          else
            []
         )
            ++ (if getLevel m >= 4 then
                    repLine m 2

                else
                    []
               )
            ++ (if getLevel m >= 6 then
                    repLine m 3

                else
                    []
               )
            ++ (if getLevel m >= 8 then
                    repLine m 4

                else
                    []
               )
            ++ (if getLevel m >= 10 then
                    repLine m 5

                else
                    []
               )
        )


{-| The full list of forms to display at any given time.
-}
getForms : Model -> List Form
getForms model =
    [ basicsForm model ]
        ++ (if getLevel model >= 2 then
                [ levelForm model ]

            else
                []
           )
        ++ [ featsForm model ]
        ++ [ kitsForm model ]
        ++ (if getLevel model >= 2 then
                [ repForm model ]

            else
                []
           )
        ++ mayList (complexOriginForm model)
        ++ mayList (customBackgroundForm model)
        ++ customOriginForm model
        ++ tacticalForms model
        ++ [ learnedSkillsForm model ]
        ++ (if getResponse model "basics-ticks" == Just "Yes" then
                [ learnedTricksForm model, extraCompsForm model ]

            else
                []
           )


{-| Update a field on the character when a form value changes, and call validation.
-}
updateFieldResponse : String -> String -> Model -> Model
updateFieldResponse key value model =
    fieldChanged key (setResponse model key value)


{-| Called when an add button on an addable form is pressed.
-}
updateExtendForm : String -> Model -> Model
updateExtendForm key model =
    case key of
        "Learned Skills" ->
            extendForm "ls-" model

        "Learned Tricks" ->
            extendForm "lt-" model

        "Extra Complications" ->
            extendForm "lc-" model

        _ ->
            model


{-| Functions for reading from the three standard extender forms.
-}
getLearnedSkills : Model -> List String
getLearnedSkills m =
    getExtendFormEntries m "ls-"


getLearnedTricks : Model -> List String
getLearnedTricks m =
    getExtendFormEntries m "lt-"


getExtraComplications : Model -> List String
getExtraComplications m =
    getExtendFormEntries m "lc-"


{-| Encodes the character hash as a Json string.
-}
encodeChar : Model -> String
encodeChar model =
    model.character
        |> Dict.toList
        |> map (\( x, y ) -> ( x, Json.Encode.string y ))
        |> Json.Encode.object
        |> Json.Encode.encode 2


{-| Deal with clicks on the "file menu". Most of these have to go to JS.
-}
handleFileCommand : String -> Model -> ( Model, Cmd Msg )
handleFileCommand x m =
    case x of
        "download" ->
            ( m, Ports.download ( "character.json", encodeChar m ) )

        "upload" ->
            ( m, Ports.doUpload 0 )

        "seturl" ->
            ( m, Ports.saveURL (encodeChar m) )

        "reset" ->
            ( { m | character = blankCharacter }, Ports.resetFileMenu 0 )

        "roll20" ->
            ( m, Ports.download ( "macros.txt", powerMacros m ) )

        "print" ->
            ( m, Ports.doPrint 0 )

        _ ->
            ( m, Ports.alert ("Invalid message " ++ x ++ " from file menu") )


{-| Decode a character hash from a JSON string.
-}
importChar : String -> Model -> ( Model, Cmd Msg )
importChar x m =
    let
        newChar =
            Json.Decode.decodeString (Json.Decode.dict Json.Decode.string) x
    in
    case newChar of
        Ok chardata ->
            ( { m | character = chardata }, Cmd.none )

        Err string ->
            ( m, Ports.alert "Error decoding character file." )


{-| Elm model update function.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormFieldUpdated k s ->
            ( updateFieldResponse k s model, Cmd.none )

        FormAddClicked f ->
            ( updateExtendForm f model, Cmd.none )

        FieldDeleteClicked f ->
            ( updateDeleteField f model, Cmd.none )

        FileCommand x ->
            handleFileCommand x model

        LoadJson x ->
            importChar x model

        _ ->
            dbUpdate msg model
