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
    { dropdownMenu : DropdownMenu.Model
    , compareCondition : Maybe ( Compare, String )
    , passValue : Maybe Int
    }


type Msg
    = NoOp
    | DropdownMenuMsg (DropdownMenu.Msg Compare)
    | PassValueChanged String


type ConfigType
    = AttackMod
    | WoundMod
    | SaveMod


init : ConfigType -> Model
init configType =
    { dropdownMenu = DropdownMenu.init
    , compareCondition = Nothing
    , passValue = Nothing
    }


update_ : Msg -> Model -> ( Model, Effect )
update_ msg model =
    case msg of
        DropdownMenuMsg dropdownMenuMsg ->
            let
                ( dropdownMenu, dropdownMenuCmd ) =
                    DropdownMenu.update
                        dropdownMenuMsg
                        model.dropdownMenu
                        |> Tuple.mapSecond (Cmd.map DropdownMenuMsg)
            in
            ( { model
                | dropdownMenu = dropdownMenu
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
            [ A.class "flex" ]
            [ DropdownMenu.view model.dropdownMenu
                { selectedLabel = Nothing
                , placeholder = "Compare condition"
                , id = config.id ++ "--compare-condition-dropdown"
                , items =
                    [ ( Eq, "Equal to" )
                    , ( Lte, "Less than or equal to" )
                    , ( Gte, "Greater than or equal to" )
                    ]
                }
                |> H.map DropdownMenuMsg
            , TextInput.view
                [ A.class <| "w2"
                ]
                PassValueChanged
            ]
        , H.div
            [ A.class "ml3"
            ]
            [ DropdownMenu.view model.dropdownMenu
                { selectedLabel = Nothing
                , placeholder = "Result modifier"
                , id = config.id ++ "--result-modifier-dropdown"
                , items =
                    [ ( Eq, "Equal to" )
                    , ( Lte, "Less than or equal to" )
                    , ( Gte, "Greater than or equal to" )
                    ]
                }
                |> H.map DropdownMenuMsg
            ]
        ]
