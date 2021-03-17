module Main exposing (..)

import Accessibility.Role exposing (dialog)
import Accessibility.Widget exposing (hasDialogPopUp, modal, required)
import Basics.Extra exposing (flip)
import Browser exposing (element)
import Button
import Dict exposing (Dict)
import Fields exposing (Fields)
import Html as H
import Html.Attributes as A
import Html.Events as E
import Html.Lazy exposing (lazy)
import Json.Decode as D
import List
import ModifierForm
import Random exposing (Seed)
import Result.Extra as ResultX
import Run exposing (Compare(..), Damage, Modifier(..))
import TextInput


type Effect
    = EffCmd (Cmd Msg)
    | EffBatch (List Effect)


fromEffect : Effect -> Cmd Msg
fromEffect eff =
    case eff of
        EffCmd cmd ->
            cmd

        EffBatch cmds ->
            List.map fromEffect cmds |> Cmd.batch


type ModifierCategory
    = WeaponSkill
    | Wound
    | Save


type Msg
    = NoOp
    | OptionsButtonClicked String
    | CloseModalButtonClicked
    | OpenModifierForm ModifierCategory
    | SubmitModifierForm ModifierCategory Modifier
    | ModifierFormMsg ModifierForm.Msg
    | UnitCountChanged String
    | AttackCountChanged String
    | ArmorPenetrationChanged String
    | DamageChanged String
    | StrengthChanged String
    | ToughnessChanged String
    | SaveChanged String
    | WeaponSkillChanged String
    | RunInput Run.Setup


type alias Flags =
    { seed : Seed
    , seeds : List Seed
    }


map10 f10 a b c d e f g h i j =
    Maybe.map5 f10 a b c d e |> Maybe.andThen (\f5 -> Maybe.map5 f5 f g h i j)


type alias Model =
    { initialized : Bool
    , flags : Flags
    , modalOpen : Bool
    , modifierCategory : ModifierCategory
    , modifierForm : ModifierForm.Model
    , weaponSkillModifier : Maybe Modifier
    , woundModifier : Maybe Modifier
    , saveModifier : Maybe Modifier
    , fields : Fields
    , result : Maybe Float
    }


modelToSetup : Model -> Maybe Run.Setup
modelToSetup model =
    map10
        Run.Setup
        (Maybe.map2 (*) (Fields.unitCountValue.get model) (Fields.attackCountValue.get model))
        (Fields.strengthValue.get model)
        (Just model.woundModifier)
        (Fields.weaponSkillValue.get model)
        (Just model.weaponSkillModifier)
        (Fields.toughnessValue.get model)
        (Fields.damageValue.get model)
        (Just <| Fields.armorPenetrationValue.get model)
        (Just model.saveModifier)
        (Fields.saveValue.get model)


init : D.Value -> ( Model, Effect )
init flagsJson =
    let
        flagsResult =
            D.decodeValue
                (D.map2
                    Flags
                    (D.at [ "seed" ] D.int |> D.map Random.initialSeed)
                    (D.at [ "seeds" ] (D.list (D.map Random.initialSeed D.int)))
                )
                flagsJson
    in
    ( { initialized = ResultX.isOk flagsResult
      , modalOpen = False
      , modifierForm = ModifierForm.init
      , flags =
            Result.withDefault
                { seed = Random.initialSeed 0
                , seeds = [ Random.initialSeed 0 ]
                }
                flagsResult
      , fields = Fields.init
      , weaponSkillModifier = Nothing
      , woundModifier = Nothing
      , saveModifier = Nothing
      , modifierCategory = Save
      , result = Nothing
      }
    , EffCmd Cmd.none
    )


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
        RunInput setup ->
            let
                results =
                    model.flags.seeds
                        |> List.map (flip Run.run setup)

                count =
                    List.length results

                sum =
                    List.foldr (+) 0 results

                average =
                    toFloat sum / toFloat count
            in
            ( { model | result = Just average }
            , EffCmd Cmd.none
            )

        WeaponSkillChanged value ->
            ( Fields.weaponSkillValue.set (String.toInt value) model
            , EffCmd Cmd.none
            )

        ToughnessChanged value ->
            ( Fields.toughnessValue.set (String.toInt value) model
            , EffCmd Cmd.none
            )

        StrengthChanged value ->
            ( Fields.strengthValue.set (String.toInt value) model
            , EffCmd Cmd.none
            )

        DamageChanged value ->
            ( Fields.damageValue.set (String.toInt value) model
            , EffCmd Cmd.none
            )

        ArmorPenetrationChanged value ->
            ( Fields.armorPenetrationValue.set (String.toInt value) model
            , EffCmd Cmd.none
            )

        UnitCountChanged value ->
            ( Fields.unitCountValue.set (String.toInt value) model
            , EffCmd Cmd.none
            )

        SaveChanged value ->
            ( Fields.saveValue.set (String.toInt value) model
            , EffCmd Cmd.none
            )

        AttackCountChanged value ->
            ( Fields.attackCountValue.set (String.toInt value) model
            , EffCmd Cmd.none
            )

        SubmitModifierForm modifierCategory modifier ->
            case modifierCategory of
                WeaponSkill ->
                    ( { model
                        | weaponSkillModifier = Just modifier
                        , modalOpen = False
                      }
                    , EffCmd Cmd.none
                    )

                Wound ->
                    ( { model
                        | woundModifier = Just modifier
                        , modalOpen = False
                      }
                    , EffCmd Cmd.none
                    )

                Save ->
                    ( { model
                        | saveModifier = Just modifier
                        , modalOpen = False
                      }
                    , EffCmd Cmd.none
                    )

        OpenModifierForm modifierCategory ->
            case modifierCategory of
                WeaponSkill ->
                    let
                        modifierForm =
                            model.weaponSkillModifier
                                |> Maybe.map ModifierForm.modifierToModel
                                |> Maybe.withDefault ModifierForm.init
                    in
                    ( { model
                        | modifierForm = modifierForm
                        , modifierCategory = modifierCategory
                        , modalOpen = True
                      }
                    , EffCmd Cmd.none
                    )

                Wound ->
                    let
                        modifierForm =
                            model.woundModifier
                                |> Maybe.map ModifierForm.modifierToModel
                                |> Maybe.withDefault ModifierForm.init
                    in
                    ( { model
                        | modifierForm = modifierForm
                        , modifierCategory = modifierCategory
                        , modalOpen = True
                      }
                    , EffCmd Cmd.none
                    )

                Save ->
                    let
                        modifierForm =
                            model.saveModifier
                                |> Maybe.map ModifierForm.modifierToModel
                                |> Maybe.withDefault ModifierForm.init
                    in
                    ( { model
                        | modifierForm = modifierForm
                        , modifierCategory = modifierCategory
                        , modalOpen = True
                      }
                    , EffCmd Cmd.none
                    )

        ModifierFormMsg modifierFormMsg ->
            let
                ( modifierForm, modifierFormCmd ) =
                    ModifierForm.update
                        modifierFormMsg
                        model.modifierForm
                        |> Tuple.mapSecond (Cmd.map ModifierFormMsg)
            in
            ( { model
                | modifierForm = modifierForm
              }
            , EffCmd modifierFormCmd
            )

        CloseModalButtonClicked ->
            ( { model
                | modalOpen = False
              }
            , EffCmd Cmd.none
            )

        OptionsButtonClicked optionId ->
            ( { model
                | modalOpen = True
              }
            , EffCmd Cmd.none
            )

        NoOp ->
            ( model, EffCmd Cmd.none )


cardView : Maybe Msg -> String -> H.Html Msg -> H.Html Msg
cardView optionsHandler optionId body =
    H.div
        [ A.class "pa2 w-50 w-33-ns w-33-m w-20-l relative" ]
        [ H.div
            [ A.class "shadow-1 bg-black-40 ph3 pv2 br3 flex justify-center"
            ]
            [ body ]
        , case optionsHandler of
            Just onOptionsClicked ->
                iconButtonView
                    [ A.class "fas fa-cogs"
                    , A.id <| optionId ++ "--modal-button"
                    , hasDialogPopUp
                    , E.onClick onOptionsClicked
                    ]
                    []

            Nothing ->
                H.text ""
        ]


iconButtonView : List (H.Attribute a) -> List (H.Html a) -> H.Html a
iconButtonView attrs children =
    H.button
        ((A.class <|
            " absolute top-0 right-0 black-80 ba b--light-blue bg-light-blue "
                ++ " hover-bg-transparent hover-white grow "
                ++ " pointer outline-0 br2 pa2 shadow-1 "
         )
            :: attrs
        )
        children


modalView : Model -> H.Html Msg
modalView model =
    let
        isOpen =
            model.modalOpen
    in
    H.div
        [ A.classList
            [ ( "fixed helvetica top-0 left-0 right-0 bg-white-30 overflow-hidden", True )
            , ( "bottom--100", isOpen == False )
            , ( "bottom-0", isOpen == True )
            ]
        , A.style "transition" ".3s"
        , dialog
        ]
        [ H.node "focus-trap"
            [ A.classList
                [ ( "relative w-90 w-70-m w-50-l center bg-black-80 br3 ba b--light-blue shadow-1", True )
                , ( "pa2 db", True )
                , ( "mt0", isOpen == False )
                , ( "mt6", isOpen == True )
                ]
            , A.style "transition" ".6s"
            , A.id "modal-view"
            , modal True
            , A.tabindex <|
                if isOpen then
                    0

                else
                    -1
            , A.attribute "active" <|
                if isOpen then
                    "true"

                else
                    "false"
            ]
            [ iconButtonView
                [ A.class "fas fa-times ph2 pv1 absolute top-0 right-0 nt2 nr2"
                , E.onClick CloseModalButtonClicked
                ]
                []
            , if isOpen then
                ModifierForm.view model.modifierForm
                    { id = "my-form"
                    , mapMsg = ModifierFormMsg
                    , onSubmit = SubmitModifierForm model.modifierCategory
                    }

              else
                H.text ""
            ]
        , if isOpen then
            H.button [ A.class "o-0" ] []

          else
            H.text ""
        ]


layout : Model -> H.Html Msg
layout model =
    H.div
        []
        [ view model
        , H.div
            [ A.class "flex justify-center flex-column items-center white-70 avenir"
            ]
            [ H.button
                ([]
                    ++ (case modelToSetup model of
                            Just setup ->
                                [ E.onClick <| RunInput setup
                                , A.class Button.baseButtonClass
                                ]

                            Nothing ->
                                [ A.class Button.disabledButtonClass
                                ]
                       )
                )
                [ H.text "Run Scenario" ]
            , case model.result of
                Just r ->
                    H.h3 [] [ H.text <| String.fromFloat r ++ " unsaved wounds" ]

                _ ->
                    H.text ""
            ]
        , modalView model
        ]


view : Model -> H.Html Msg
view model =
    H.div
        [ A.class "helvetica pa3 white-80 flex flex-wrap flex-row center-m"
        ]
        [ cardView (Just <| OpenModifierForm WeaponSkill) model.fields.weaponSkill.id <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "3+"
                    , A.id model.fields.weaponSkill.id
                    ]
                    WeaponSkillChanged
                , H.br [] []
                , H.label
                    [ A.class "f7 fw5"
                    , A.for model.fields.weaponSkill.id
                    ]
                    [ H.text model.fields.weaponSkill.label
                    ]
                ]
        , cardView Nothing model.fields.unitCount.id <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "2"
                    , A.id model.fields.unitCount.id
                    ]
                    UnitCountChanged
                , H.br [] []
                , H.label
                    [ A.class "f7 fw5"
                    , A.for model.fields.unitCount.id
                    ]
                    [ H.text model.fields.unitCount.label ]
                ]
        , cardView Nothing model.fields.attackCount.id <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "2"
                    , A.id model.fields.attackCount.id
                    ]
                    AttackCountChanged
                , H.br [] []
                , H.label
                    [ A.class "f7 fw5"
                    , A.for model.fields.attackCount.id
                    ]
                    [ H.text model.fields.attackCount.label ]
                ]
        , cardView (Just <| OpenModifierForm Wound) model.fields.strength.id <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "4"
                    , A.id model.fields.strength.id
                    ]
                    StrengthChanged
                , H.br [] []
                , H.label
                    [ A.class "f7 fw5"
                    , A.for model.fields.strength.id
                    ]
                    [ H.text model.fields.strength.label ]
                ]
        , cardView Nothing model.fields.armorPenetration.id <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "-2"
                    , A.id model.fields.armorPenetration.id
                    ]
                    ArmorPenetrationChanged
                , H.br [] []
                , H.label
                    [ A.class "f7 fw5"
                    , A.for model.fields.armorPenetration.id
                    ]
                    [ H.text model.fields.armorPenetration.label ]
                ]
        , cardView Nothing model.fields.damage.id <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "D3"
                    , A.id model.fields.damage.id
                    ]
                    DamageChanged
                , H.br [] []
                , H.label
                    [ A.class "f7 fw5"
                    , A.for model.fields.damage.id
                    ]
                    [ H.text model.fields.damage.label ]
                ]
        , cardView Nothing model.fields.toughness.id <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "4"
                    , A.id model.fields.toughness.id
                    ]
                    ToughnessChanged
                , H.br [] []
                , H.label
                    [ A.class "f7 fw5"
                    , A.for model.fields.toughness.id
                    ]
                    [ H.text model.fields.toughness.label ]
                ]
        , cardView (Just <| OpenModifierForm Save) model.fields.save.id <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "5+"
                    , A.id model.fields.save.id
                    ]
                    SaveChanged
                , H.br [] []
                , H.label
                    [ A.class "f7 fw5"
                    , A.for model.fields.save.id
                    ]
                    [ H.text model.fields.save.label ]
                ]
        ]


main : Program D.Value Model Msg
main =
    element
        { update = \msg model -> update msg model |> Tuple.mapSecond fromEffect
        , init = init >> Tuple.mapSecond fromEffect
        , subscriptions = \_ -> Sub.none
        , view = layout
        }
