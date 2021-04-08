module MainTest exposing (..)

import Accessibility.Role exposing (menu)
import Accessibility.Widget exposing (hasMenuPopUp)
import Die
import Expect exposing (fail, pass)
import Html as H
import Json.Decode as D
import Json.Encode as E
import Main exposing (..)
import Maybe.Extra exposing (isJust)
import Result.Extra as ResultX
import Test exposing (..)
import Test.Html.Event as Event exposing (click)
import Test.Html.Query as Query
import Test.Html.Selector exposing (Selector, attribute, containing, id, text)


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
    ( "change"
    , E.object
        [ ( "target"
          , E.object
                [ ( "value", E.string value )
                ]
          )
        ]
    )


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


suite : Test
suite =
    describe "The main module"
        [ test "The user can fill out the form and submit" <|
            \_ ->
                initTestApp
                    |> (\testApp ->
                            userInteraction testApp [ id testApp.model.fields.weaponSkill.id ] (changeEvent "3")
                       )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ id testApp.model.fields.unitCount.id ] (changeEvent "10")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ id testApp.model.fields.attackCount.id ] (changeEvent "2")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ id testApp.model.fields.strength.id ] (changeEvent "4")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ id testApp.model.fields.toughness.id ] (changeEvent "4")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ id testApp.model.fields.save.id ] (changeEvent "3")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp [ id testApp.model.fields.damage.id ] (changeEvent "1")
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
                                , id "modifier-form--compare-condition-value"
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
                                , id "modifier-form--subsequent--value-mod"
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
                            userInteraction testApp [ id testApp.model.fields.unitCount.id ] (changeEvent "10")
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
        ]
