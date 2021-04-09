module Run exposing (..)

import Die exposing (Compare(..), Die, DieState(..), Modifier(..), rollDie)
import Random


type alias Setup =
    { attacks : Int
    , strength : Int
    , woundModifier : Maybe Modifier
    , weaponSkill : Int
    , weaponSkillModifier : Maybe Modifier
    , toughness : Int
    , damage : FixedOrRoll
    , armorPenetration : Maybe Int
    , saveModifier : Maybe Modifier
    , save : Int
    }


type FixedOrRoll
    = Fixed Int
    | Roll Int


type Phase
    = Attack (List Die)
    | Wound (List Die)
    | Save (List Die)
    | Damage (List Die)
    | Resolve Int


woundPassValue : Int -> Int -> Int
woundPassValue strength toughness =
    if strength >= toughness * 2 then
        2

    else if strength > toughness then
        3

    else if strength == toughness then
        4

    else if strength * 2 <= toughness then
        6

    else
        5


run : Random.Seed -> Setup -> Int
run seed setup =
    run_ seed setup (Attack []) (Wound []) 0


run_ : Random.Seed -> Setup -> Phase -> Phase -> Int -> Int
run_ seed setup phase nextPhase currentDamage =
    case ( phase, nextPhase ) of
        ( Attack dice, Wound woundDice ) ->
            if setup.attacks > 0 then
                run_
                    seed
                    { setup | attacks = setup.attacks - 1 }
                    (Attack <| Die 6 setup.weaponSkill NotRolled setup.weaponSkillModifier setup.attacks :: dice)
                    nextPhase
                    0

            else
                case dice of
                    [] ->
                        run_ seed setup nextPhase (Save []) 0

                    currentRoll :: nextRolls ->
                        let
                            ( nextSeed, nextDie, nextMod ) =
                                rollDie currentRoll seed

                            withSetupMod =
                                Batch
                                    [ MaybeMod nextMod
                                    , MaybeMod setup.woundModifier
                                    ]

                            nextWounds =
                                case nextDie.state of
                                    Passed _ ->
                                        Die 6
                                            (woundPassValue setup.strength setup.toughness)
                                            NotRolled
                                            (Just withSetupMod)
                                            nextDie.id
                                            :: woundDice

                                    _ ->
                                        woundDice
                        in
                        run_
                            nextSeed
                            setup
                            (Attack nextRolls)
                            (Wound nextWounds)
                            0

        ( Wound dice, Save saveDice ) ->
            case dice of
                [] ->
                    run_
                        seed
                        setup
                        (Save saveDice)
                        (Damage [])
                        0

                currentRoll :: nextRolls ->
                    let
                        ( nextSeed, nextDie, nextMod ) =
                            rollDie currentRoll seed

                        withSetupMod =
                            Batch
                                [ MaybeMod nextMod
                                , SubtractValue (setup.armorPenetration |> Maybe.map abs |> Maybe.withDefault 0)
                                , MaybeMod setup.saveModifier
                                ]
                                |> Just

                        nextSaves =
                            case nextDie.state of
                                Passed _ ->
                                    Die 6 setup.save NotRolled withSetupMod nextDie.id :: saveDice

                                _ ->
                                    saveDice
                    in
                    run_
                        nextSeed
                        setup
                        (Wound nextRolls)
                        (Save nextSaves)
                        0

        ( Save dice, Damage damageDice ) ->
            case dice of
                [] ->
                    run_ seed setup (Damage damageDice) (Resolve currentDamage) currentDamage

                currentRoll :: nextRolls ->
                    let
                        ( nextSeed, nextDie, nextMod ) =
                            rollDie currentRoll seed

                        ( nextDamageDice, nextDamage ) =
                            case nextDie.state of
                                Failed _ ->
                                    case setup.damage of
                                        Fixed val ->
                                            ( damageDice, currentDamage + val )

                                        Roll val ->
                                            ( Die val 1 NotRolled nextMod nextDie.id :: damageDice, currentDamage )

                                _ ->
                                    ( damageDice, currentDamage )
                    in
                    run_
                        nextSeed
                        setup
                        (Save nextRolls)
                        (Damage nextDamageDice)
                        nextDamage

        ( Damage dice, Resolve woundCount ) ->
            case dice of
                [] ->
                    run_
                        seed
                        setup
                        (Resolve woundCount)
                        (Resolve woundCount)
                        0

                currentRoll :: nextRolls ->
                    let
                        ( nextSeed, nextDie, _ ) =
                            rollDie currentRoll seed

                        rollVal =
                            case nextDie.state of
                                Passed val ->
                                    val

                                _ ->
                                    -1000
                    in
                    run_
                        nextSeed
                        setup
                        (Damage nextRolls)
                        (Resolve <| woundCount + rollVal)
                        0

        ( Resolve result, _ ) ->
            result

        _ ->
            -1

