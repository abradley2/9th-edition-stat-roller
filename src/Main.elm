module Main exposing (..)

import Accessibility.Role exposing (dialog)
import Accessibility.Widget exposing (hasDialogPopUp, modal, required)
import Basics.Extra exposing (flip)
import Browser exposing (element)
import Browser.Dom as Dom
import Html as H
import Html.Attributes as A
import Html.Events as E
import Html.Lazy exposing (lazy)
import Json.Decode as D
import ModifierForm
import Random exposing (Seed)
import Result.Extra as ResultX
import Run exposing (Compare(..), Modifier(..))
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
    | ElementFocused (Result Dom.Error ())
    | OptionsButtonClicked String
    | CloseModalButtonClicked
    | OpenModifierForm ModifierCategory
    | SubmitModifierForm ModifierCategory Modifier
    | ModifierFormMsg ModifierForm.Msg


type alias Flags =
    { seed : Seed
    , seeds : List Seed
    }


map8 f8 a b c d e f g h =
    Maybe.map4 f8 a b c d
        |> Maybe.andThen (\f4 -> Maybe.map4 f4 e f g h)


type alias Model =
    { initialized : Bool
    , flags : Flags
    , modalOpen : Bool
    , modifierCategory : ModifierCategory
    , modifierForm : ModifierForm.Model
    , weaponSkill : Maybe Int
    , attackingUnits : Maybe Int
    , attacksPerUnit : Maybe Int
    , strength : Maybe Int
    , armorPenetration : Maybe Int
    , damage : Maybe Int
    , toughness : Maybe Int
    , save : Maybe Int
    , weaponSkillModifier : Maybe Modifier
    , woundModifier : Maybe Modifier
    , saveModifier : Maybe Modifier
    }


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
      , armorPenetration = Nothing
      , attackingUnits = Nothing
      , attacksPerUnit = Nothing
      , damage = Nothing
      , save = Nothing
      , strength = Nothing
      , toughness = Nothing
      , weaponSkill = Nothing
      , weaponSkillModifier = Nothing
      , woundModifier = Nothing
      , saveModifier = Nothing
      , modifierCategory = Save
      }
    , EffCmd Cmd.none
    )


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
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

        ElementFocused _ ->
            ( model, EffCmd Cmd.none )

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
        , modalView model
        ]


view : Model -> H.Html Msg
view model =
    H.div
        [ A.class "helvetica pa3 white-80 flex flex-wrap flex-row center-m"
        ]
        [ cardView (Just <| OpenModifierForm WeaponSkill) "weapon-skill" <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "3+"
                    , A.id "weapon-skill"
                    ]
                    (always NoOp)
                , H.br [] []
                , H.label
                    [ A.class "f7 fw5"
                    , A.for "weapon-skill"
                    ]
                    [ H.text "Weapon/Ballistic Skill"
                    ]
                ]
        , cardView Nothing "attacks-per-unit" <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "2"
                    , A.id "attacks-per-unit"
                    ]
                    (always NoOp)
                , H.br [] []
                , H.label
                    [ A.class "f7 fw5"
                    , A.for "attacks-per-unit"
                    ]
                    [ H.text "Number of Attacking Units" ]
                ]
        , cardView Nothing "attacks-per-weapon" <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "2"
                    , A.id "attacks-per-weapon"
                    ]
                    (always NoOp)
                , H.br [] []
                , H.label
                    [ A.class "f7 fw5"
                    , A.for "attacks-per-weapon"
                    ]
                    [ H.text "Attacks per Unit" ]
                ]
        , cardView (Just <| OpenModifierForm Wound) "strength" <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "4"
                    , A.id "strength"
                    ]
                    (always NoOp)
                , H.br [] []
                , H.label
                    [ A.class "f7 fw5"
                    , A.for "strength"
                    ]
                    [ H.text "Strength" ]
                ]
        , cardView Nothing "armor-penetration" <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "-2"
                    , A.id "armor-penetration"
                    ]
                    (always NoOp)
                , H.br [] []
                , H.label
                    [ A.class "f7 fw5"
                    , A.for "armor-penetration"
                    ]
                    [ H.text "Armor Penetration" ]
                ]
        , cardView Nothing "damage-dice" <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "D3"
                    , A.id "damage-dice"
                    ]
                    (always NoOp)
                , H.br [] []
                , H.label
                    [ A.class "f7 fw5"
                    , A.for "damage-dice"
                    ]
                    [ H.text "Damage" ]
                ]
        , cardView Nothing "toughness" <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "4"
                    , A.id "toughness"
                    ]
                    (always NoOp)
                , H.br [] []
                , H.label
                    [ A.class "f7 fw5"
                    , A.for "toughness"
                    ]
                    [ H.text "Toughness" ]
                ]
        , cardView (Just <| OpenModifierForm Save) "save" <|
            H.div
                [ A.class "pv2 inline-flex flex-column items-center" ]
                [ TextInput.view
                    [ required True
                    , A.class "w3"
                    , A.placeholder "5+"
                    , A.id "save"
                    ]
                    (always NoOp)
                , H.br [] []
                , H.label
                    [ A.class "f7 fw5"
                    , A.for "save"
                    ]
                    [ H.text "Armor/Invulnerable Save" ]
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
