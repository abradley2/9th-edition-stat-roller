module MainTest exposing (..)

import Accessibility.Role exposing (menu)
import Accessibility.Widget exposing (hasMenuPopUp)
import Die exposing (Compare(..))
import DropdownMenu
import Expect exposing (fail, pass)
import Html as H
import Html.Attributes as A
import Json.Decode as D
import Json.Encode as E
import Main exposing (..)
import Maybe.Extra exposing (isJust)
import PresetsForm exposing (attackerPresets, defenderPresets)
import Result.Extra as ResultX
import Run exposing (woundPassValue)
import Test exposing (..)
import Test.Html.Event as Event exposing (click)
import Test.Html.Query as Query
import Test.Html.Selector exposing (Selector, attribute, containing, id, text)
import Fields exposing (armorPenetration)
import ModifierForm


type alias TestApp =
    { model : Model
    , view : H.Html Msg
    }


initTestApp : TestApp
initTestApp =
    let
        ( model, _ ) =
            init flagsJson
    in
    TestApp model (view model)


flagsJson : D.Value
flagsJson =
    E.object
        [ ( "seed", E.int 1234 )
        , ( "seeds"
          , E.list E.int [ 1234, 1234, 1234 ]
          )
        ]


changeEvent : String -> ( String, D.Value )
changeEvent value =
    ( "newval"
    , E.object
        [ ( "detail"
          , E.string value
          )
        ]
    )


queryPath : H.Html a -> List Selector -> Query.Single a
queryPath html selectors =
    List.foldl
        (\selector query -> Query.find [ selector ] query)
        (Query.fromHtml html)
        selectors


userInteraction : TestApp -> List Selector -> ( String, D.Value ) -> Result String TestApp
userInteraction testApp selectors ev =
    selectors
        |> List.foldl
            (\selector query -> Query.find [ selector ] query)
            (Query.fromHtml testApp.view)
        |> Event.simulate ev
        |> Event.toResult
        |> Result.map (\model -> update model testApp.model |> Tuple.first)
        |> Result.map (\model -> TestApp model (view model))


inputId = A.attribute "input-id" >> attribute

suite : Test
suite =
    describe "The main module"
        [ test "The user can fill out the form and submit" <|
            \_ ->
                initTestApp
                    |> (\testApp ->
                            userInteraction testApp [ inputId testApp.model.fields.weaponSkill.id ] (changeEvent "3")
                       )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ inputId testApp.model.fields.unitCount.id ] (changeEvent "10")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ inputId testApp.model.fields.attackCount.id ] (changeEvent "2")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ inputId testApp.model.fields.strength.id ] (changeEvent "4")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ inputId testApp.model.fields.toughness.id ] (changeEvent "4")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ inputId testApp.model.fields.save.id ] (changeEvent "3")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ inputId testApp.model.fields.damage.id ] (changeEvent "1")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ id "submit-button" ] click
                        )
                    |> Result.map
                        (Expect.all
                            [ .model >> .fields >> .weaponSkill >> .value >> Expect.equal (Just 3)
                            , .model >> .fields >> .unitCount >> .value >> Expect.equal (Just 10)
                            , .model >> .fields >> .attackCount >> .value >> Expect.equal (Just 2)
                            , .model >> .fields >> .strength >> .value >> Expect.equal (Just 4)
                            , .model >> .fields >> .toughness >> .value >> Expect.equal (Just 4)
                            , .model >> .fields >> .save >> .value >> Expect.equal (Just 3)
                            , .model >> .result >> isJust >> Expect.true "Result should be filled"
                            ]
                        )
                    |> Result.mapError fail
                    |> ResultX.merge
        , test "The user can fill out the form and submit (alternate values" <|
            \_ ->
                initTestApp
                    |> (\testApp ->
                            userInteraction testApp [ inputId testApp.model.fields.weaponSkill.id ] (changeEvent "3")
                       )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ inputId testApp.model.fields.unitCount.id ] (changeEvent "10")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ inputId testApp.model.fields.attackCount.id ] (changeEvent "2")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ inputId testApp.model.fields.strength.id ] (changeEvent "4")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ inputId testApp.model.fields.toughness.id ] (changeEvent "4")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ inputId testApp.model.fields.armorPenetration.id ] (changeEvent "-3")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ inputId testApp.model.fields.save.id ] (changeEvent "3")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ inputId testApp.model.fields.damage.id ] (changeEvent "D3")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ id "submit-button" ] click
                        )
                    |> Result.map
                        (Expect.all
                            [ .model >> .fields >> .weaponSkill >> .value >> Expect.equal (Just 3)
                            , .model >> .fields >> .unitCount >> .value >> Expect.equal (Just 10)
                            , .model >> .fields >> .attackCount >> .value >> Expect.equal (Just 2)
                            , .model >> .fields >> .strength >> .value >> Expect.equal (Just 4)
                            , .model >> .fields >> .toughness >> .value >> Expect.equal (Just 4)
                            , .model >> .fields >> .save >> .value >> Expect.equal (Just 3)
                            , .model >> .fields >> .armorPenetration >> .value >> Expect.equal (Just 3)
                            , .model >> .fields >> .damage >> .value >> Expect.equal (Just <| Run.Roll 3)
                            , .model >> .result >> isJust >> Expect.true "Result should be filled"
                            ]
                        )
                    |> Result.mapError fail
                    |> ResultX.merge
        , test "The user can fill out the modifier form and apply it to a field" <|
            \_ ->
                initTestApp
                    |> (\testApp ->
                            userInteraction testApp [ id "strength--modal-button" ] click
                       )
                    |> Result.mapError ((++) "Failed to find button for opening modal: ")
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp
                                [ id "modifier-form--compare-condition-dropdown"
                                , attribute hasMenuPopUp
                                ]
                                click
                        )
                    |> Result.mapError ((++) "Failed to find first compare condition dropdown: ")
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp
                                [ id "modifier-form--compare-condition-dropdown"
                                , attribute menu
                                , containing [ text "Equal to" ]
                                ]
                                click
                        )
                    |> Result.mapError ((++) "Failed to find first compare condition option: ")
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp
                                [ id "modal-view"
                                , inputId "modifier-form--compare-condition-value"
                                ]
                                (changeEvent "6")
                        )
                    |> Result.mapError ((++) "Failed to find first compare condition value: ")
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp
                                [ id "modal-view"
                                , id "modifier-form--result-modifier-dropdown"
                                , attribute hasMenuPopUp
                                ]
                                click
                        )
                    |> Result.mapError ((++) "Failed to find first result dropdown: ")
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp
                                [ id "modal-view"
                                , id "modifier-form--result-modifier-dropdown"
                                , attribute menu
                                , containing
                                    [ text "Apply modifier to save"
                                    ]
                                ]
                                click
                        )
                    |> Result.mapError ((++) "Failed to find first result option: ")
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp
                                [ id "modal-view"
                                , id "modifier-form--subsequent--compare-condition-dropdown"
                                , attribute hasMenuPopUp
                                ]
                                click
                        )
                    |> Result.mapError ((++) "Failed to find second compare condition dropdown: ")
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp
                                [ id "modal-view"
                                , id "modifier-form--subsequent--compare-condition-dropdown"
                                , attribute menu
                                , containing
                                    [ text "Always"
                                    ]
                                ]
                                click
                        )
                    |> Result.mapError ((++) "Failed to find second compare condition option: ")
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp
                                [ id "modal-view"
                                , id "modifier-form--subsequent--result-modifier-dropdown"
                                , attribute hasMenuPopUp
                                ]
                                click
                        )
                    |> Result.mapError ((++) "Failed to find second result dropdown: ")
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp
                                [ id "modal-view"
                                , id "modifier-form--subsequent--result-modifier-dropdown"
                                , attribute menu
                                , containing
                                    [ text "Modify roll: subtract"
                                    ]
                                ]
                                click
                        )
                    |> Result.mapError ((++) "Failed to find second result option: ")
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp
                                [ id "modal-view"
                                , inputId "modifier-form--subsequent--value-mod"
                                ]
                                (changeEvent "3")
                        )
                    |> Result.mapError ((++) "Failed to find second result value: ")
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp
                                [ id "modal-view"
                                , id "apply-modifier-button"
                                ]
                                click
                        )
                    |> Result.map
                        (Expect.all
                            [ .model
                                >> .woundModifier
                                >> Expect.equal
                                    (Just <|
                                        Die.Compare Die.Eq
                                            6
                                            (Die.InfluenceNext <|
                                                Die.Compare Die.Always 1 (Die.SubtractValue 3)
                                            )
                                    )
                            ]
                        )
                    |> Result.mapError fail
                    |> ResultX.merge
        , test "The user can utilize the preset form to quickly get going" <|
            \_ ->
                initTestApp
                    |> (\testApp ->
                            userInteraction testApp
                                [ id "preset-form"
                                , id "attacker-preset-dropdown"
                                , attribute menu
                                , containing
                                    [ text "Craftworld Guardian"
                                    ]
                                ]
                                click
                       )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp
                                [ id "preset-form"
                                , id "defender-preset-dropdown"
                                , attribute menu
                                , containing
                                    [ text "Plague Marine"
                                    ]
                                ]
                                click
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ inputId testApp.model.fields.unitCount.id ] (changeEvent "10")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ id "submit-button" ] click
                        )
                    |> Result.map
                        (\testApp ->
                            Expect.true "Expected result to be a Just value" <| isJust testApp.model.result
                        )
                    |> Result.mapError fail
                    |> ResultX.merge
        , test "Wound pass value" <|
            \_ ->
                Expect.all
                    [ woundPassValue 2 >> Expect.equal 6
                    , woundPassValue 3 >> Expect.equal 5
                    , woundPassValue 4 >> Expect.equal 4
                    , woundPassValue 5 >> Expect.equal 3
                    , woundPassValue 8 >> Expect.equal 2
                    ]
                    4
        , test "The defender presets dropdown works for all possible options" <|
            \_ ->
                initTestApp
                    |> (\testApp ->
                            defenderPresets
                                |> List.map
                                    (\( presetName, presetConfig ) ->
                                        userInteraction testApp
                                            [ id "preset-form"
                                            , id "defender-preset-dropdown"
                                            , attribute menu
                                            , containing
                                                [ text presetName
                                                ]
                                            ]
                                            click
                                            |> Result.map
                                                (\nextTestApp ->
                                                    always (Expect.equal (presetConfig testApp.model) nextTestApp.model)
                                                )
                                    )
                                |> ResultX.combine
                       )
                    |> Result.map (\extList -> Expect.all extList ())
                    |> Result.mapError fail
                    |> ResultX.merge
        , test "The attacker presets dropdown works for all possible options" <|
            \_ ->
                initTestApp
                    |> (\testApp ->
                            attackerPresets
                                |> List.map
                                    (\( presetName, presetConfig ) ->
                                        userInteraction testApp
                                            [ id "preset-form"
                                            , id "attacker-preset-dropdown"
                                            , attribute menu
                                            , containing
                                                [ text presetName
                                                ]
                                            ]
                                            click
                                            |> Result.map
                                                (\nextTestApp ->
                                                    always (Expect.equal (presetConfig testApp.model) nextTestApp.model)
                                                )
                                    )
                                |> ResultX.combine
                       )
                    |> Result.map (\extList -> Expect.all extList ())
                    |> Result.mapError fail
                    |> ResultX.merge
        , test "The modifier form will pre-populate when there is an applied attack preset that contains a modifier" <|
            \_ ->
                initTestApp
                    |> (\testApp ->
                            userInteraction testApp
                                [ id "preset-form"
                                , id "attacker-preset-dropdown"
                                , attribute hasMenuPopUp
                                ]
                                click
                       )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp
                                [ id "preset-form"
                                , id "defender-preset-dropdown"
                                , attribute hasMenuPopUp
                                ]
                                click
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp
                                [ id "preset-form"
                                , id "attacker-preset-dropdown"
                                , attribute menu
                                , containing
                                    [ text "Craftworld Guardian"
                                    ]
                                ]
                                click
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp
                                [ id <| testApp.model.fields.strength.id ++ "--modal-button"
                                ]
                                click
                        )
                    |> Result.map
                        (\testApp ->
                            queryPath testApp.view
                                [ id "modal-view"
                                , id "modifier-form--compare-condition-dropdown"
                                , id "modifier-form--compare-condition-dropdown--button"
                                ]
                                |> Query.contains [ H.text "Equal to" ]
                        )
                    |> Result.mapError fail
                    |> ResultX.merge
        , test "Compare labels read as expected" <|
            \_ -> Expect.all
                [ (\fn -> fn Lte) >> Expect.equal "Less than or equal to"
                , (\fn -> fn Gte) >> Expect.equal "Greater than or equal to"
                , (\fn -> fn Always) >> Expect.equal "Always"
                , (\fn -> fn Eq) >> Expect.equal "Equal to"
                ]
                ModifierForm.compareLabel
        ]
