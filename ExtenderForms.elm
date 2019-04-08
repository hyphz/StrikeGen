module ExtenderForms exposing (..)

import Dict
import ModelDB exposing (..)
import String
import FormsModel exposing (..)
import List


{-| Add a new entry to a variable length form.
-}
extendForm : String -> Model -> Model
extendForm prefix model =
    let
        oldCount =
            (getResponseInt model (prefix ++ "count") 0)

        newCount =
            (String.fromInt (oldCount + 1))

        countUpdated =
            setResponse model (prefix ++ "count") newCount

        newMemberKey =
            prefix ++ newCount
    in
        setResponse countUpdated newMemberKey ""


{-| Closes gaps in the numbering order when an item on an addable form is
deleted.
-}
closeGaps_ : String -> Int -> Int -> Model -> Model
closeGaps_ prefix current total m =
    let
        addPrefix t =
            prefix ++ (String.fromInt t)
    in
        if current > total then
            m
        else
            case (Dict.get (addPrefix current) m.character) of
                Just _ ->
                    closeGaps_ prefix (current + 1) total m

                Nothing ->
                    case (Dict.get (addPrefix (current + 1)) m.character) of
                        Just _ ->
                            closeGaps_ prefix current total (moveResponse m (addPrefix (current + 1)) (addPrefix current))

                        Nothing ->
                            ModelDB.blankModel


closeGaps : String -> Int -> Model -> Model
closeGaps prefix total m =
    closeGaps_ prefix 1 total m


{-| Called when the delete button on an addable form is pressed.
-}
updateDeleteField : String -> Model -> Model
updateDeleteField key model =
    let
        keyhyphenindex =
            String.indexes "-" key

        realKeyHyphenIndex =
            case (List.head keyhyphenindex) of
                Nothing ->
                    0

                Just x ->
                    x

        keyPrehyphen =
            (String.slice 0 realKeyHyphenIndex key) ++ "-"

        countName =
            keyPrehyphen ++ "count"

        removeResponse =
            killResponse model key

        checkResponseRemoved =
            case (Dict.get key removeResponse.character) of
                Nothing ->
                    removeResponse

                Just _ ->
                    ModelDB.blankModel

        reduceCount =
            (setResponse checkResponseRemoved countName (String.fromInt ((getResponseInt checkResponseRemoved countName 0) - 1)))
    in
        closeGaps keyPrehyphen (getResponseInt reduceCount countName 0) reduceCount


{-| Get all the entries on an extender form, as a list.
-}
getExtendFormEntries : Model -> String -> List String
getExtendFormEntries m prefix =
    case (getResponse m (prefix ++ "count")) of
        Nothing ->
            []

        Just s ->
            case (String.toInt s) of
                Nothing ->
                    [ "BUG! Mangled ExtendForm Counter " ++ prefix ]

                Just itemCount ->
                    List.map (\x -> Maybe.withDefault "BUG! Broken ExtendForm Entry" (getResponse m (prefix ++ (String.fromInt x)))) (List.range 1 (itemCount))


{-| One entry in an extender form. Takes the model, the name of the field,
the prefix for the form's keys and the field number.
-}
extenderFormEntry : Model -> String -> String -> Int -> Field
extenderFormEntry m name prefix x =
    FreeformField { name = name, del = True, key = (prefix ++ "-" ++ (String.fromInt x)) }


{-| Creates an extendable form with insert/delete buttons.
-}
extenderForm : Model -> String -> String -> String -> Form
extenderForm m header itemname key =
    Form True
        header
        (List.map (extenderFormEntry m itemname key) (List.range 1 (getResponseInt m (key ++ "-count") 0)))
