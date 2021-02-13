module Run exposing (..)

import Random


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

        _ ->
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
