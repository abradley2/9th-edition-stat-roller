module Main exposing (..)

import Accessibility.Role exposing (dialog)
import Accessibility.Widget exposing (hasDialogPopUp, modal, required)
import Basics.Extra exposing (flip)
import Button
import FieldParser exposing (formatArmorPenetration, formatFixedOrRoll, formatPassValue, parseArmorPenetration, parseFixedOrRoll, parsePassValue)
import Fields exposing (Fields)
import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as D
import List
import Maybe.Extra exposing (isJust)
import ModifierForm exposing (formatModifier)
import Random exposing (Seed)
import Result.Extra as ResultX
import Die exposing (Compare(..), Modifier(..))
import Run
import TextInput


attackerPresets : List ( String, Model -> Model )
attackerPresets =
    [ ( "Craftworld Guardians"
      , Fields.weaponSkillValue.set (Just 3)
            >> Fields.strengthValue.set (Just 4)
            >> Fields.damageValue.set (Just <| Run.Fixed 1)
            >> (\model -> { model | woundModifier = Just shurikenWeapons })
      )
    ]


defenderPresets : List ( String, Model -> Model )
defenderPresets =
    [ ( "Plague Marines"
      , Fields.toughnessValue.set (Just 4)
            >> Fields.saveValue.set (Just 3)
            >> (\model -> { model | saveModifier = Just <| disgustinglyReslient 3 })
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
        (Die.InfluenceNext <|
            Die.Compare Die.Always 0 (Die.Reroll (Just 5))
        )


weaponSkillPresets : List ( String, Modifier )
weaponSkillPresets =
    []


woundPresets : List Modifier
woundPresets =
    [-- add one for shuriken weapons
    ]


type ModifierCategory
    = WeaponSkill
    | Wound
    | Save


type Msg
    = NoOp
    | CloseModalButtonClicked
    | OpenModifierForm ModifierCategory
    | SubmitModifierForm ModifierCategory Modifier
    | ClearModifier ModifierCategory
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


init : D.Value -> ( Model, Cmd Msg )
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
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClearModifier category ->
            case category of
                WeaponSkill ->
                    ( { model | weaponSkillModifier = Nothing }, Cmd.none )

                Wound ->
                    ( { model | woundModifier = Nothing }, Cmd.none )

                Save ->
                    ( { model | saveModifier = Nothing }, Cmd.none )

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
            , Cmd.none
            )

        WeaponSkillChanged value ->
            ( Fields.weaponSkillValue.set (parsePassValue value) model
            , Cmd.none
            )

        ToughnessChanged value ->
            ( Fields.toughnessValue.set (parsePassValue value) model
            , Cmd.none
            )

        StrengthChanged value ->
            ( Fields.strengthValue.set (String.toInt value) model
            , Cmd.none
            )

        DamageChanged value ->
            ( Fields.damageValue.set (parseFixedOrRoll value) model
            , Cmd.none
            )

        ArmorPenetrationChanged value ->
            ( Fields.armorPenetrationValue.set (parseArmorPenetration value) model
            , Cmd.none
            )

        UnitCountChanged value ->
            ( Fields.unitCountValue.set (String.toInt value) model
            , Cmd.none
            )

        SaveChanged value ->
            ( Fields.saveValue.set (parsePassValue value) model
            , Cmd.none
            )

        AttackCountChanged value ->
            ( Fields.attackCountValue.set (String.toInt value) model
            , Cmd.none
            )

        SubmitModifierForm modifierCategory modifier ->
            case modifierCategory of
                WeaponSkill ->
                    ( { model
                        | weaponSkillModifier = Just modifier
                        , modalOpen = False
                      }
                    , Cmd.none
                    )

                Wound ->
                    ( { model
                        | woundModifier = Just modifier
                        , modalOpen = False
                      }
                    , Cmd.none
                    )

                Save ->
                    ( { model
                        | saveModifier = Just modifier
                        , modalOpen = False
                      }
                    , Cmd.none
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
                    , Cmd.none
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
                    , Cmd.none
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
                    , Cmd.none
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
            , modifierFormCmd
            )

        CloseModalButtonClicked ->
            ( { model
                | modalOpen = False
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


cardView : Maybe Msg -> String -> H.Html Msg -> H.Html Msg
cardView optionsHandler optionId body =
    H.div
        [ A.class "pa2 w-50 w-33-ns w-33-m w-20-l relative" ]
        [ H.div
            [ A.class "shadow-1 bg-black-40 ph3 pt2 pb3 br3 flex justify-center"
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
                ModifierForm.view
                    (case model.modifierCategory of
                        WeaponSkill ->
                            Just "wound"

                        Wound ->
                            Just "save"

                        _ ->
                            Nothing
                    )
                    model.modifierForm
                    { id = "modifier-form"
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


presetSelectionView : Model -> H.Html Msg
presetSelectionView model =
    H.div
        []
        []


layout : Model -> H.Html Msg
layout model =
    H.div
        []
        [ presetSelectionView model
        , view model
        , modifierListView model
        , H.div
            [ A.class "flex justify-center flex-column items-center white-70 avenir"
            ]
            [ H.button
                (A.id "submit-button"
                    :: (case modelToSetup model of
                            Just setup ->
                                [ E.onClick <| RunInput setup
                                , A.class Button.baseButtonClass
                                , A.id "submit-button"
                                ]

                            Nothing ->
                                [ A.class Button.disabledButtonClass
                                , A.id "submit-button"
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


clearModifierButton : Msg -> H.Html Msg
clearModifierButton onClick =
    H.div
        [ A.class "absolute bottom-0 left-0 right-0 flex justify-center"
        ]
        [ H.button
            [ E.onClick onClick
            , A.class Button.textButtonClass
            , A.class "nt4"
            ]
            [ H.text "Clear Applied Modifier"
            , H.i
                [ A.class "pl2 fas fa-cogs" ]
                []
            ]
        ]


modifierListView : Model -> H.Html Msg
modifierListView model =
    let
        modDesc =
            [ Maybe.map (formatModifier "Wound" >> (++) "On Roll to Hit: " >> H.text) model.weaponSkillModifier
            , Maybe.map (formatModifier "Save" >> (++) "On Roll to Wound: " >> H.text) model.woundModifier
            , Maybe.map (formatModifier "" >> (++) "On Roll to Save: " >> H.text) model.saveModifier
            ]

        modCount =
            List.filter isJust modDesc |> List.length
    in
    H.div
        [ A.class "avenir white-70 tc pb2"
        ]
        [ H.div
            [ A.class "tc" ]
            [ H.text "Modifiers Applied:" ]
        , H.div
            [ A.class "tc pv1 f7 white lh-copy" ]
            [ if modCount == 0 then
                H.span [] [ H.text "None" ]

              else
                H.div []
                    (List.map (Maybe.withDefault (H.text "")) modDesc
                        |> List.intersperse (H.br [] [])
                    )
            ]
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
                    , A.placeholder "4+"
                    , A.id model.fields.weaponSkill.id
                    ]
                    (Fields.weaponSkillValue.get model |> Maybe.map formatPassValue)
                    WeaponSkillChanged
                , H.label
                    [ A.class "f7 fw5 pt2"
                    , A.for model.fields.weaponSkill.id
                    ]
                    [ H.text model.fields.weaponSkill.label
                    ]
                , case model.weaponSkillModifier of
                    Just _ ->
                        clearModifierButton (ClearModifier WeaponSkill)

                    Nothing ->
                        H.text ""
                ]
        , cardView Nothing model.fields.unitCount.id <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "5"
                    , A.id model.fields.unitCount.id
                    ]
                    (Fields.unitCountValue.get model |> Maybe.map String.fromInt)
                    UnitCountChanged
                , H.label
                    [ A.class "f7 fw5 pt2"
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
                    (Fields.attackCountValue.get model |> Maybe.map String.fromInt)
                    AttackCountChanged
                , H.label
                    [ A.class "f7 fw5 pt2"
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
                    , A.placeholder "3"
                    , A.id model.fields.strength.id
                    ]
                    (Fields.strengthValue.get model |> Maybe.map String.fromInt)
                    StrengthChanged
                , H.label
                    [ A.class "f7 fw5 pt2"
                    , A.for model.fields.strength.id
                    ]
                    [ H.text model.fields.strength.label ]
                , case model.woundModifier of
                    Just _ ->
                        clearModifierButton (ClearModifier Wound)

                    Nothing ->
                        H.text ""
                ]
        , cardView Nothing model.fields.armorPenetration.id <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "-1"
                    , A.id model.fields.armorPenetration.id
                    ]
                    (Fields.armorPenetrationValue.get model |> Maybe.map formatArmorPenetration)
                    ArmorPenetrationChanged
                , H.label
                    [ A.class "f7 fw5 pt2"
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
                    (Fields.damageValue.get model |> Maybe.map formatFixedOrRoll)
                    DamageChanged
                , H.label
                    [ A.class "f7 fw5 pt2"
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
                    (Fields.toughnessValue.get model |> Maybe.map String.fromInt)
                    ToughnessChanged
                , H.label
                    [ A.class "f7 fw5 pt2"
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
                    (Fields.saveValue.get model |> Maybe.map formatPassValue)
                    SaveChanged
                , H.label
                    [ A.class "f7 fw5 pt2"
                    , A.for model.fields.save.id
                    ]
                    [ H.text model.fields.save.label ]
                , case model.saveModifier of
                    Just _ ->
                        clearModifierButton (ClearModifier Save)

                    _ ->
                        H.text ""
                ]
        ]
