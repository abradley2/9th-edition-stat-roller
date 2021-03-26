module MainTest exposing (..)

import Expect exposing (Expectation, fail, pass)
import Fuzz exposing (Fuzzer, int, list, string)
import Html as H
import Json.Decode as D
import Json.Encode as E
import Main exposing (..)
import Result.Extra as ResultX
import Test exposing (..)
import Test.Html.Event as Event exposing (Event, click, input)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (Selector, attribute, id)


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


userInteraction : TestApp -> Selector -> ( String, D.Value ) -> Result String TestApp
userInteraction testApp sel ev =
    testApp.view
        |> Query.fromHtml
        |> Query.find [ sel ]
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
                            userInteraction testApp (id testApp.model.fields.weaponSkill.id) (changeEvent "3")
                       )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp (id testApp.model.fields.unitCount.id) (changeEvent "10")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp (id testApp.model.fields.attackCount.id) (changeEvent "2")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp (id testApp.model.fields.strength.id) (changeEvent "4")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp (id testApp.model.fields.toughness.id) (changeEvent "4")
                        )
                    |> Result.andThen
                        (\testApp ->
                            userInteraction testApp (id testApp.model.fields.save.id) (changeEvent "3")
                        )
                    |> Result.map
                        (Expect.all
                            [ .model >> .fields >> .weaponSkill >> .value >> Expect.equal (Just 3)
                            , .model >> .fields >> .unitCount >> .value >> Expect.equal (Just 10)
                            , .model >> .fields >> .attackCount >> .value >> Expect.equal (Just 2)
                            , .model >> .fields >> .strength >> .value >> Expect.equal (Just 4)
                            , .model >> .fields >> .toughness >> .value >> Expect.equal (Just 4)
                            , .model >> .fields >> .save >> .value >> Expect.equal (Just 3)
                            ]
                        )
                    |> Result.mapError fail
                    |> ResultX.merge
        ]
