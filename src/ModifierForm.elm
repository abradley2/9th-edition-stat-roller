module ModifierForm exposing (..)

import DropdownMenu
import Html as H
import Html.Attributes as A
import Html.Events as E
import Run exposing (Compare(..))
import TextInput


type Effect
    = EffCmd (Cmd Msg)
    | EffBatch (List Effect)


fromEffect : Effect -> Cmd Msg
fromEffect eff =
    case eff of
        EffCmd cmd ->
            cmd

        EffBatch effList ->
            effList
                |> List.map fromEffect
                |> Cmd.batch


type alias Model =
    { compareDropdownMenu : DropdownMenu.Model
    , compareCondition : Maybe ( Compare, String )
    , resultDropdownMenu : DropdownMenu.Model
    , resultModifier : Maybe ( ResultType, String )
    , passValue : Maybe Int
    }


type ResultType
    = Reroll
    | RerollNew
    | InfluenceNext


type Msg
    = NoOp
    | CompareMenuMsg (DropdownMenu.Msg Compare)
    | ResultMenuMsg (DropdownMenu.Msg ResultType)
    | PassValueChanged String


type ConfigType
    = AttackMod
    | WoundMod
    | SaveMod


init : ConfigType -> Model
init configType =
    { compareDropdownMenu = DropdownMenu.init
    , compareCondition = Nothing
    , resultDropdownMenu = DropdownMenu.init
    , resultModifier = Nothing
    , passValue = Nothing
    }


update_ : Msg -> Model -> ( Model, Effect )
update_ msg model =
    case msg of
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
            , EffCmd dropdownMenuCmd
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
              }
            , EffCmd dropdownMenuCmd
            )

        _ ->
            ( model, EffCmd Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    update_ msg model |> Tuple.mapSecond fromEffect


type alias Config =
    { id : String
    }


view : Model -> Config -> H.Html Msg
view model config =
    H.div
        [ A.class <| "flex flex-wrap"
        ]
        [ H.div
            [ A.class "flex ml3 mt3" ]
            [ DropdownMenu.view model.compareDropdownMenu
                { selectedLabel = Maybe.map Tuple.second model.compareCondition
                , placeholder = "Compare condition"
                , id = config.id ++ "--compare-condition-dropdown"
                , items =
                    [ ( Eq, "Equal to" )
                    , ( Lte, "Less than or equal to" )
                    , ( Gte, "Greater than or equal to" )
                    ]
                }
                |> H.map CompareMenuMsg
            , TextInput.view
                [ A.class <| "w2"
                ]
                PassValueChanged
            ]
        , H.div
            [ A.class "flex ml3 mt3"
            ]
            [ DropdownMenu.view model.resultDropdownMenu
                { selectedLabel = Maybe.map Tuple.second model.resultModifier
                , placeholder = "Result modifier"
                , id = config.id ++ "--result-modifier-dropdown"
                , items =
                    [ ( Reroll, "Re-roll die" )
                    , ( RerollNew, "Re-roll with new die" )
                    , ( InfluenceNext, "Apply modifier to next roll" )
                    ]
                }
                |> H.map ResultMenuMsg
            ]
        ]
