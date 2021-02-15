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
import TextInput
import Random exposing (Seed)
import Result.Extra as ResultX
import Task


type Effect
    = EffCmd (Cmd Msg)
    | EffFocusElement String
    | EffBatch (List Effect)


fromEffect : Effect -> Cmd Msg
fromEffect eff =
    case eff of
        EffCmd cmd ->
            cmd

        EffFocusElement elementId ->
            Task.attempt ElementFocused (Dom.focus elementId)

        EffBatch cmds ->
            List.map fromEffect cmds |> Cmd.batch


type Msg
    = NoOp
    | ElementFocused (Result Dom.Error ())
    | OptionsButtonClicked String
    | CloseModalButtonClicked
    | ModifierFormMsg ModifierForm.Msg


type alias Flags =
    { seed : Seed
    , seeds : List Seed
    }


type alias Model =
    { initialized : Bool
    , flags : Flags
    , modalOpen : ( String, Bool )
    , modifierForm : ModifierForm.Model
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
      , modalOpen = ( "", False )
      , modifierForm = ModifierForm.init ModifierForm.AttackMod
      , flags =
            Result.withDefault
                { seed = Random.initialSeed 0
                , seeds = [ Random.initialSeed 0 ]
                }
                flagsResult
      }
    , EffCmd Cmd.none
    )


update : Msg -> Model -> ( Model, Effect )
update msg model =
    case msg of
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
                | modalOpen = ( "", False )
              }
            , EffFocusElement <| Tuple.first model.modalOpen
            )

        OptionsButtonClicked optionId ->
            ( { model
                | modalOpen = ( optionId, True )
              }
            , EffFocusElement "modal-view"
            )

        NoOp ->
            ( model, EffCmd Cmd.none )


cardView : String -> H.Html Msg -> H.Html Msg
cardView optionId body =
    H.div
        [ A.class "pa2 w-50 w-33-ns w-33-m w-20-l relative" ]
        [ H.div
            [ A.class "shadow-1 bg-black-40 ph3 pv2 br3 flex justify-center"
            ]
            [ body ]
        , iconButtonView
            [ A.class "fas fa-cogs"
            , A.id <| optionId ++ "--modal-button"
            , hasDialogPopUp
            , E.onClick <| OptionsButtonClicked (optionId ++ "--modal-button")
            ]
            []
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
            Tuple.second model.modalOpen
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
                H.map
                    ModifierFormMsg
                    (ModifierForm.view model.modifierForm { id = "my-form" })

              else
                H.text ""
            ]
        , if isOpen then
            H.button [ A.class "o-0" ] []

          else
            H.text ""
        ]


resultDisplay : List Seed -> H.Html Msg
resultDisplay seeds =
    let
        allResult =
            seeds
                |> List.map (flip run <| sampleSetup)

        total =
            toFloat (List.foldr (+) 0 allResult) / toFloat (List.length allResult)
    in
    H.text ""


layout : Model -> H.Html Msg
layout model =
    H.div
        []
        [ lazy resultDisplay model.flags.seeds
        , view model
        , modalView model
        ]


view : Model -> H.Html Msg
view model =
    H.div
        [ A.class "helvetica pa3 white-80 flex flex-wrap flex-row center-m"
        ]
        [ cardView "weapon-skill" <|
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
        , cardView "attacks-per-unit" <|
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
        , cardView "attacks-per-weapon" <|
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
        , cardView "strength" <|
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
        , cardView "armor-penetration" <|
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
        , cardView "damage-dice" <|
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
        , cardView "toughness" <|
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
        , cardView "save" <|
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


type alias Die =
    { sides : Int
    , passValue : Int
    , modifier : Maybe Modifier
    }


type Modifier
    = NoMod
    | AddValue Int
    | SubtractValue Int
    | InfluenceNext Modifier
    | Reroll
    | Compare Compare Int Modifier
    | MaybeMod (Maybe Modifier)
    | Batch (List Modifier)


type Compare
    = Lte
    | Gte
    | Eq


rollDie : Random.Seed -> Die -> ( Int, Random.Seed )
rollDie seed die =
    Random.step (Random.int 1 die.sides) seed


applyModifier : Die -> Modifier -> ( Int, Random.Seed ) -> ( Random.Seed, Int, Maybe Modifier )
applyModifier die modifier ( currentVal, seed ) =
    case modifier of
        MaybeMod mMod ->
            mMod
                |> Maybe.map (\mod -> applyModifier die mod ( currentVal, seed ))
                |> Maybe.withDefault ( seed, currentVal, Nothing )

        Compare Lte val nextMod ->
            if currentVal <= val then
                applyModifier die nextMod ( currentVal, seed )

            else
                ( seed, currentVal, Nothing )

        Compare Gte val nextMod ->
            if currentVal >= val then
                applyModifier die nextMod ( currentVal, seed )

            else
                ( seed, currentVal, Nothing )

        Compare Eq val nextMod ->
            if currentVal == val then
                applyModifier die nextMod ( currentVal, seed )

            else
                ( seed, currentVal, Nothing )

        AddValue plusVal ->
            ( seed, currentVal + plusVal, Nothing )

        SubtractValue minusVal ->
            ( seed, currentVal - minusVal, Nothing )

        Reroll ->
            let
                ( nextVal, nextSeed ) =
                    rollDie seed die
            in
            ( nextSeed, nextVal, Nothing )

        InfluenceNext nextMod ->
            ( seed, currentVal, Just nextMod )

        NoMod ->
            ( seed, currentVal, Nothing )

        Batch modlist ->
            List.foldr
                (\mod ( curSeed, curVal, curMod ) ->
                    applyModifier die mod ( curVal, curSeed )
                        |> (\( nextSeed, nextVal, nextMod ) ->
                                ( nextSeed, nextVal, Just <| Batch [ MaybeMod curMod, MaybeMod nextMod ] )
                           )
                )
                ( seed, currentVal, Nothing )
                modlist


type alias Setup =
    { attacks : Int
    , attackModifier : Maybe Modifier
    , strength : Int
    , strengthModifier : Maybe Modifier
    , weaponSkill : Int
    , weaponSkillModifier : Maybe Modifier
    , toughness : Int
    , damage : Int
    , armorPenetration : Int
    , save : Int
    }


type Damage
    = Fixed Int
    | Roll Int


type Phase
    = Attack (List Die)
    | Wound (List Die)
    | Save (List Die)
    | Damage (List Die)
    | Resolve Int


woundPassValue : Setup -> Int
woundPassValue setup =
    if setup.strength >= setup.toughness * 2 then
        2

    else if setup.strength > setup.toughness then
        3

    else if setup.strength == setup.toughness then
        4

    else if setup.strength * 2 <= setup.toughness then
        6

    else
        5


run_ : Random.Seed -> Setup -> Phase -> Phase -> Int
run_ seed setup phase nextPhase =
    case ( phase, nextPhase ) of
        ( Attack dice, Wound woundDice ) ->
            if setup.attacks > 0 then
                run_
                    seed
                    { setup | attacks = setup.attacks - 1 }
                    (Attack <| Die 6 setup.weaponSkill setup.weaponSkillModifier :: dice)
                    nextPhase

            else
                case dice of
                    [] ->
                        run_ seed setup nextPhase (Save [])

                    currentRoll :: nextRolls ->
                        let
                            ( rollValue_, nextSeed_ ) =
                                rollDie seed currentRoll

                            ( nextSeed, rollValue, nextMod ) =
                                setup.attackModifier
                                    |> Maybe.map (\mod -> applyModifier currentRoll mod ( rollValue_, nextSeed_ ))
                                    |> Maybe.withDefault ( nextSeed_, rollValue_, Nothing )

                            nextWounds =
                                if rollValue >= currentRoll.passValue then
                                    Die 6 (woundPassValue setup) nextMod :: woundDice

                                else
                                    woundDice
                        in
                        run_
                            nextSeed
                            setup
                            (Attack nextRolls)
                            (Wound nextWounds)

        ( Wound dice, Save saveDice ) ->
            case dice of
                [] ->
                    run_
                        seed
                        setup
                        (Save saveDice)
                        (Damage [])

                currentRoll :: nextRolls ->
                    let
                        ( nextSeed, rollValue, nextMod ) =
                            rollDie seed currentRoll
                                |> applyModifier currentRoll (MaybeMod currentRoll.modifier)

                        modWithAp =
                            Batch
                                [ MaybeMod nextMod
                                , SubtractValue setup.armorPenetration
                                ]
                                |> Just

                        nextSaves =
                            if rollValue >= currentRoll.passValue then
                                Die 6 setup.save modWithAp :: saveDice

                            else
                                saveDice
                    in
                    run_
                        nextSeed
                        setup
                        (Wound nextRolls)
                        (Save nextSaves)

        ( Save dice, Damage damageDice ) ->
            case dice of
                [] ->
                    run_ seed setup (Damage damageDice) (Resolve 0)

                currentRoll :: nextRolls ->
                    let
                        ( nextSeed, rollValue, nextMod ) =
                            rollDie seed currentRoll
                                |> applyModifier currentRoll (MaybeMod currentRoll.modifier)

                        nextDamageDice =
                            if rollValue < currentRoll.passValue then
                                Die setup.damage 0 nextMod :: damageDice

                            else
                                damageDice
                    in
                    run_
                        nextSeed
                        setup
                        (Save nextRolls)
                        (Damage nextDamageDice)

        ( Damage dice, Resolve woundCount ) ->
            case dice of
                [] ->
                    run_
                        seed
                        setup
                        (Resolve woundCount)
                        (Resolve woundCount)

                currentRoll :: nextRolls ->
                    let
                        ( nextSeed, rollVal, _ ) =
                            rollDie seed currentRoll
                                |> applyModifier currentRoll (MaybeMod currentRoll.modifier)
                    in
                    run_
                        nextSeed
                        setup
                        (Damage nextRolls)
                        (Resolve <| woundCount + rollVal)

        ( Resolve result, _ ) ->
            result

        ( a, b ) ->
            let
                c =
                    Debug.log "phase = " ( printPhase a, printPhase b )
            in
            -1


printPhase : Phase -> String
printPhase p =
    case p of
        Attack _ ->
            "Attack"

        Wound _ ->
            "Wound"

        Save _ ->
            "Save"

        Damage _ ->
            "Damage"

        Resolve _ ->
            "Resolve"


sampleSetup : Setup
sampleSetup =
    Setup
        20
        Nothing
        4
        Nothing
        3
        Nothing
        4
        1
        0
        3


run : Random.Seed -> Setup -> Int
run seed setup =
    run_ seed setup (Attack []) (Wound [])
