module PresetsForm exposing (..)

import Accessibility as H
import Die exposing (Modifier)
import DropdownMenu
import Fields exposing (Fields)
import Html.Attributes as A
import Html.Events as E
import List.Extra as ListX
import Run exposing (FixedOrRoll(..))


type Dropdown
    = AttackerDropdown
    | DefenderDropdown


type alias Model =
    { attackerDropdown : DropdownMenu.Model
    , defenderDropdown : DropdownMenu.Model
    }


type Msg
    = DropdownMsg Dropdown (DropdownMenu.Msg String)


init : Model
init =
    { attackerDropdown = DropdownMenu.init
    , defenderDropdown = DropdownMenu.init
    }


update : Msg -> Model -> ( Model, Cmd Msg, PartialModel a -> PartialModel a )
update msg model =
    case msg of
        DropdownMsg AttackerDropdown dropdownMsg ->
            let
                ( attackerDropdown, attackerDropdownCmd ) =
                    DropdownMenu.update
                        dropdownMsg
                        model.attackerDropdown
                        |> Tuple.mapSecond (Cmd.map (DropdownMsg AttackerDropdown))

                appliedPreset =
                    case dropdownMsg of
                        DropdownMenu.ItemSelected ( presetName, _ ) _ ->
                            attackerPresets
                                |> ListX.find (Tuple.first >> (==) presetName)
                                |> Maybe.map Tuple.second
                                |> Maybe.withDefault (\v -> v)

                        _ ->
                            \v -> v
            in
            ( { model | attackerDropdown = attackerDropdown }, attackerDropdownCmd, appliedPreset )

        DropdownMsg DefenderDropdown dropdownMsg ->
            let
                ( defenderDropdown, defenderDropdownCmd ) =
                    DropdownMenu.update
                        dropdownMsg
                        model.defenderDropdown
                        |> Tuple.mapSecond (Cmd.map (DropdownMsg DefenderDropdown))

                appliedPreset =
                    case dropdownMsg of
                        DropdownMenu.ItemSelected ( presetName, _ ) _ ->
                            defenderPresets
                                |> ListX.find (Tuple.first >> (==) presetName)
                                |> Maybe.map Tuple.second
                                |> Maybe.withDefault (\v -> v)

                        _ ->
                            \v -> v
            in
            ( { model | defenderDropdown = defenderDropdown }, defenderDropdownCmd, appliedPreset )


type alias PartialModel a =
    { a
        | fields : Fields
        , saveModifier : Maybe Modifier
        , woundModifier : Maybe Modifier
        , weaponSkillModifier : Maybe Modifier
    }


view : Model -> H.Html Msg
view model =
    H.div
        [ A.class "flex justify-center pt4"
        ]
        [ H.div
            []
            [ DropdownMenu.view
                model.attackerDropdown
                { label = "Attacker Presets"
                , selectedLabel = Just "Choose a preset"
                , id = "attacker-preset-dropdown"
                , items = List.map (\( l, _ ) -> ( l, l )) attackerPresets
                }
                |> H.map (DropdownMsg AttackerDropdown)
            ]
        , H.div
            [ A.class "ml4" ]
            [ DropdownMenu.view
                model.defenderDropdown
                { label = "Defender Presets"
                , selectedLabel = Just "Choose a preset"
                , id = "defender-preset-dropdown"
                , items = List.map (\( l, _ ) -> ( l, l )) defenderPresets
                }
                |> H.map (DropdownMsg DefenderDropdown)
            ]
        ]


attackerPresets : List ( String, PartialModel a -> PartialModel a )
attackerPresets =
    [ ( "Craftworld Guardian"
      , Fields.weaponSkillValue.set (Just 3)
            >> Fields.strengthValue.set (Just 4)
            >> Fields.damageValue.set (Just <| Run.Fixed 1)
            >> Fields.attackCountValue.set (Just 2)
            >> Fields.armorPenetrationValue.set Nothing
            >> (\model -> { model | weaponSkillModifier = Nothing })
            >> (\model -> { model | woundModifier = Just shurikenWeapons })
      )
    , ( "Wraithblade w/ Ghostaxe"
      , Fields.weaponSkillValue.set (Just 4)
            >> Fields.strengthValue.set (Just 7)
            >> Fields.damageValue.set (Just (Roll 3))
            >> Fields.attackCountValue.set (Just 2)
            >> Fields.armorPenetrationValue.set (Just 3)
            >> (\model -> { model | weaponSkillModifier = Nothing })
            >> (\model -> { model | woundModifier = Nothing })
      )
    ]


defenderPresets : List ( String, PartialModel a -> PartialModel a )
defenderPresets =
    [ ( "Plague Marine"
      , Fields.toughnessValue.set (Just 4)
            >> Fields.saveValue.set (Just 3)
            >> (\model -> { model | saveModifier = Just <| disgustinglyReslient 3 })
      )
    , ( "Primaris Intercessor"
      , Fields.toughnessValue.set (Just 4)
            >> Fields.saveValue.set (Just 3)
      )
    ]


shurikenWeapons : Modifier
shurikenWeapons =
    Die.Compare Die.Eq
        6
        (Die.InfluenceNext <|
            Die.Compare Die.Always 0 (Die.SubtractValue 3)
        )


disgustinglyReslient : Int -> Modifier
disgustinglyReslient save =
    Die.Compare Die.Lte
        (save - 1)
        (Die.Reroll (Just 5))


weaponSkillPresets : List ( String, Modifier )
weaponSkillPresets =
    []


woundPresets : List Modifier
woundPresets =
    [-- add one for shuriken weapons
    ]
