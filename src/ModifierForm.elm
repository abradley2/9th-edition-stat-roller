module ModifierForm exposing (..)

import Accessibility exposing (fieldset)
import Accessibility.Widget exposing (disabled)
import Button
import DropdownMenu
import FieldParser exposing (formatPassValue, parsePassValue)
import Html as H
import Html.Attributes as A
import Html.Events as E
import Run exposing (Compare(..), Modifier(..))
import TextInput


formatModifier : String -> Modifier -> String
formatModifier nextPhase mod =
    case mod of
        Run.AddValue val ->
            "Add " ++ String.fromInt val

        Run.SubtractValue val ->
            "Subtract " ++ String.fromInt val

        Run.Reroll mVal ->
            case mVal of
                Just val ->
                    "Roll for " ++ formatPassValue val

                Nothing ->
                    "Re-roll"

        Run.Compare cmp val nextMod ->
            case cmp of
                Always ->
                    "Always " ++ formatModifier nextPhase nextMod

                Eq ->
                    formatModifier nextPhase nextMod ++ " when Equal to " ++ String.fromInt val

                Lte ->
                    formatModifier nextPhase nextMod ++ " when Less Than or Equal to " ++ String.fromInt val

                Gte ->
                    formatModifier nextPhase nextMod ++ " when Greater Than or Equal to " ++ String.fromInt val

        Run.InfluenceNext nextMod ->
            formatModifier "" nextMod ++ " on " ++ nextPhase

        _ -> ""

type alias Model_ =
    { compareDropdownMenu : DropdownMenu.Model
    , compareCondition : Maybe ( Compare, String )
    , passValue : Maybe Int
    , resultDropdownMenu : DropdownMenu.Model
    , resultModifier : Maybe ( ResultType, String )
    , newPassValue : Maybe Int
    , nextModifierForm : Maybe Model
    , valueMod : Maybe Int
    }


modifierToModel : Modifier -> Model
modifierToModel modifier =
    Model <|
        case modifier of
            NoMod ->
                init_

            Compare compare passValue resultMod ->
                let
                    nextModifierForm =
                        case resultMod of
                            Run.InfluenceNext nextMod ->
                                nextMod
                                    |> modifierToModel
                                    |> Just

                            _ ->
                                Nothing

                    ( resultType, valueMod, newPassValue ) =
                        case resultMod of
                            Run.AddValue value ->
                                ( ValueMod Add, Just value, Nothing )

                            Run.SubtractValue value ->
                                ( ValueMod Subtract, Just value, Nothing )

                            Run.Reroll (Just value) ->
                                ( RerollNew, Nothing, Just value )

                            Run.Reroll Nothing ->
                                ( Reroll, Nothing, Nothing )

                            _ ->
                                ( InfluenceNext, Nothing, Nothing )

                    baseModel =
                        init_
                in
                { baseModel
                    | passValue = Just passValue
                    , valueMod = valueMod
                    , newPassValue = newPassValue
                    , compareCondition = Just ( compare, compareLabel compare )
                    , nextModifierForm = nextModifierForm
                    , resultModifier =
                        Just
                            ( resultType
                            , resultTypeLabel Nothing resultType
                            )
                }

            _ ->
                init_


modelToModifier : Model_ -> Maybe Modifier
modelToModifier model =
    let
        compare =
            model.compareCondition
                |> Maybe.map Tuple.first
                |> Maybe.andThen
                    (\cmp ->
                        case cmp of
                            Always ->
                                Just <| Compare Run.Always 1

                            _ ->
                                Maybe.map
                                    (Compare cmp)
                                    model.passValue
                    )

        result =
            model.resultModifier
                |> Maybe.map Tuple.first
                |> Maybe.andThen
                    (\resMod ->
                        case resMod of
                            Reroll ->
                                Just (Run.Reroll Nothing)

                            RerollNew ->
                                Maybe.map (Just >> Run.Reroll) model.newPassValue

                            ValueMod Add ->
                                Maybe.map Run.AddValue model.valueMod

                            ValueMod Subtract ->
                                Maybe.map Run.SubtractValue model.valueMod

                            InfluenceNext ->
                                Maybe.andThen modelToModifier
                                    (case model.nextModifierForm of
                                        Just (Model model_) ->
                                            Just model_

                                        Nothing ->
                                            Nothing
                                    )
                                    |> Maybe.map Run.InfluenceNext
                    )
    in
    Maybe.map2
        (\f x -> f x)
        compare
        result


type Model
    = Model Model_


type ValueMod
    = Add
    | Subtract


type ResultType
    = Reroll
    | RerollNew
    | InfluenceNext
    | ValueMod ValueMod


type Msg
    = NoOp
    | CompareMenuMsg (DropdownMenu.Msg Compare)
    | ResultMenuMsg (DropdownMenu.Msg ResultType)
    | PassValueChanged String
    | NewPassValueChanged String
    | ValueModChanged String
    | NextModifierMsg Msg


init_ : Model_
init_ =
    { compareDropdownMenu = DropdownMenu.init
    , compareCondition = Nothing
    , resultDropdownMenu = DropdownMenu.init
    , resultModifier = Nothing
    , passValue = Nothing
    , newPassValue = Nothing
    , valueMod = Nothing
    , nextModifierForm = Nothing
    }


init : Model
init =
    Model init_


update_ : Msg -> Model_ -> ( Model_, Cmd Msg )
update_ msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ValueModChanged valueMod ->
            ( { model | valueMod = String.toInt valueMod }
            , Cmd.none
            )

        PassValueChanged passValue ->
            ( { model | passValue = String.toInt passValue }
            , Cmd.none
            )

        NewPassValueChanged newPassValue ->
            ( { model | newPassValue = parsePassValue newPassValue }
            , Cmd.none
            )

        NextModifierMsg nextModifierMsg ->
            case model.nextModifierForm of
                Just (Model model_) ->
                    let
                        ( nextModifierForm, nextModifierCmd ) =
                            update_ nextModifierMsg model_
                                |> Tuple.mapSecond (Cmd.map NextModifierMsg)
                    in
                    ( { model
                        | nextModifierForm = Just (Model nextModifierForm)
                      }
                    , nextModifierCmd
                    )

                _ ->
                    ( model, Cmd.none )

        CompareMenuMsg dropdownMenuMsg ->
            let
                ( compareDropdownMenu, dropdownMenuCmd ) =
                    DropdownMenu.update
                        dropdownMenuMsg
                        model.compareDropdownMenu
                        |> Tuple.mapSecond (Cmd.map CompareMenuMsg)
            in
            ( { model
                | compareDropdownMenu = compareDropdownMenu
                , compareCondition =
                    case dropdownMenuMsg of
                        DropdownMenu.ItemSelected compareCondition _ ->
                            Just compareCondition

                        _ ->
                            model.compareCondition
              }
            , dropdownMenuCmd
            )

        ResultMenuMsg dropdownMenuMsg ->
            let
                ( resultDropdownMenu, dropdownMenuCmd ) =
                    DropdownMenu.update
                        dropdownMenuMsg
                        model.resultDropdownMenu
                        |> Tuple.mapSecond (Cmd.map ResultMenuMsg)
            in
            ( { model
                | resultDropdownMenu = resultDropdownMenu
                , resultModifier =
                    case dropdownMenuMsg of
                        DropdownMenu.ItemSelected resultModifier _ ->
                            Just resultModifier

                        _ ->
                            model.resultModifier
                , nextModifierForm =
                    case dropdownMenuMsg of
                        DropdownMenu.ItemSelected ( InfluenceNext, _ ) _ ->
                            Just <| Model init_

                        DropdownMenu.ItemSelected _ _ ->
                            Nothing

                        _ ->
                            model.nextModifierForm
              }
            , dropdownMenuCmd
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Model model_ ->
            update_ msg model_
                |> Tuple.mapFirst Model


type alias Config a =
    { id : String
    , onSubmit : Modifier -> a
    , mapMsg : Msg -> a
    }


view : Maybe String -> Model -> Config a -> H.Html a
view nextPhase model config =
    case model of
        Model model_ ->
            H.div
                []
                [ view_ nextPhase model_ config False
                    |> H.map config.mapMsg
                , H.div
                    [ A.class "white pa3"
                    ]
                    [ H.button
                        ((A.id "apply-modifier-button")
                            :: (case modelToModifier model_ of
                                    Just mod ->
                                        [ disabled False
                                        , A.class Button.baseButtonClass
                                        , E.onClick <| config.onSubmit mod
                                        ]

                                    Nothing ->
                                        [ disabled True
                                        , A.class Button.disabledButtonClass
                                        ]
                               )
                        )
                        [ H.text "Apply Modifier" ]
                    ]
                ]


withLabel : String -> String -> H.Html Msg -> H.Html Msg
withLabel fieldId label field =
    H.div
        [ A.class "flex flex-column"
        ]
        [ field
        , H.label
            [ A.for fieldId
            , A.class "white-80 f7 pl2 pt2"
            ]
            [ H.text label ]
        ]


view_ : Maybe String -> Model_ -> Config a -> Bool -> H.Html Msg
view_ nextPhase model config nested =
    let
        id =
            if nested then
                config.id ++ "--subsequent"

            else
                config.id
    in
    H.div
        [ A.class <| "flex flex-wrap"
        ]
        [ fieldset
            [ A.class "flex items-start ml3 mt3"
            ]
            [ DropdownMenu.view model.compareDropdownMenu
                { selectedLabel = Maybe.map Tuple.second model.compareCondition
                , label = "Compare condition"
                , id = id ++ "--compare-condition-dropdown"
                , items =
                    [ ( Always, "Always" )
                    , ( Eq, "Equal to" )
                    , ( Lte, "Less than or equal to" )
                    , ( Gte, "Greater than or equal to" )
                    ]
                }
                |> H.map CompareMenuMsg
            , case model.compareCondition of
                Just ( Always, _ ) ->
                    H.text ""

                Nothing ->
                    H.text ""

                _ ->
                    TextInput.view
                        [ A.class <| "w3"
                        , A.id (id ++ "--compare-condition-value")
                        ]
                        (model.passValue |> Maybe.map String.fromInt)
                        PassValueChanged
                        |> withLabel (id ++ "--compare-condition-value") "Value"
            ]
        , fieldset
            [ A.class "flex items-start ml3 mt3"
            ]
            [ DropdownMenu.view model.resultDropdownMenu
                { selectedLabel = Maybe.map Tuple.second model.resultModifier
                , label = "Result modifier"
                , id = id ++ "--result-modifier-dropdown"
                , items =
                    [ Reroll
                    , RerollNew
                    , ValueMod Add
                    , ValueMod Subtract
                    ]
                        ++ (case ( nested, nextPhase ) of
                                ( False, Just _ ) ->
                                    [ InfluenceNext ]

                                _ ->
                                    []
                           )
                        |> List.map (\v -> ( v, resultTypeLabel nextPhase v ))
                }
                |> H.map ResultMenuMsg
            , case model.resultModifier of
                Just ( ValueMod _, _ ) ->
                    TextInput.view
                        [ A.class "w3"
                        , A.id (id ++ "--value-mod")
                        ]
                        (model.valueMod |> Maybe.map String.fromInt)
                        ValueModChanged
                        |> withLabel (id ++ "--value-mod") "Value"

                Just ( RerollNew, _ ) ->
                    TextInput.view
                        [ A.class "w3"
                        , A.id (id ++ "--new-pass-value")
                        ]
                        (model.newPassValue |> Maybe.map formatPassValue)
                        NewPassValueChanged
                        |> withLabel (id ++ "--new-pass-value") "Pass value"

                _ ->
                    H.text ""
            ]
        , case model.nextModifierForm of
            Just (Model model_) ->
                H.div [] [ view_ nextPhase model_ config True ]
                    |> H.map NextModifierMsg

            Nothing ->
                H.text ""
        ]


compareLabel : Compare -> String
compareLabel compare =
    case compare of
        Always ->
            "Always"

        Eq ->
            "Equal to"

        Lte ->
            "Less than or equal to"

        Gte ->
            "Greater than or equal to"


resultTypeLabel : Maybe String -> ResultType -> String
resultTypeLabel nextPhase resultType =
    case resultType of
        Reroll ->
            "Re-roll dice"

        RerollNew ->
            "Re-roll new dice"

        ValueMod Add ->
            "Modify roll: add"

        ValueMod Subtract ->
            "Modify roll: subtract"

        InfluenceNext ->
            "Apply modifier to " ++ Maybe.withDefault "" nextPhase
