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
    | Reroll (Maybe Int)
    | Compare Compare Int Modifier
    | MaybeMod (Maybe Modifier)
    | Batch (List Modifier)


type Compare
    = Always
    | Lte
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

        Compare Always _ nextMod ->
            applyModifier die nextMod ( currentVal, seed )

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

        Reroll mNewVal ->
            let
                nextDie =
                    mNewVal
                        |> Maybe.map (\passValue -> { die | passValue = passValue })
                        |> Maybe.withDefault die

                ( nextVal, nextSeed ) =
                    rollDie seed nextDie


                -- TODO: I hate this
                modVal = Maybe.map (\diff -> nextVal - (diff - die.passValue))  mNewVal |> Maybe.withDefault nextVal
            in
            ( nextSeed, modVal, Nothing )

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
                    (Attack <| Die 6 setup.weaponSkill setup.weaponSkillModifier :: dice)
                    nextPhase
                    0

            else
                case dice of
                    [] ->
                        run_ seed setup nextPhase (Save []) 0

                    currentRoll :: nextRolls ->
                        let
                            ( rollValue_, nextSeed_ ) =
                                rollDie seed currentRoll

                            ( nextSeed, rollValue, nextMod ) =
                                currentRoll.modifier
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
                        ( nextSeed, rollValue, nextMod ) =
                            rollDie seed currentRoll
                                |> applyModifier currentRoll
                                    (Batch
                                        [ MaybeMod currentRoll.modifier
                                        , MaybeMod setup.woundModifier
                                        ]
                                    )

                        modWithAp =
                            Batch
                                [ MaybeMod nextMod
                                , SubtractValue (setup.armorPenetration |> Maybe.withDefault 0)
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
                        0

        ( Save dice, Damage damageDice ) ->
            case dice of
                [] ->
                    run_ seed setup (Damage damageDice) (Resolve currentDamage) currentDamage

                currentRoll :: nextRolls ->
                    let
                        ( nextSeed, rollValue, nextMod ) =
                            rollDie seed currentRoll
                                |> applyModifier currentRoll
                                    (Batch
                                        [ MaybeMod currentRoll.modifier
                                        , MaybeMod setup.saveModifier
                                        ]
                                    )

                        (nextDamageDice, nextDamage) =
                            if rollValue < currentRoll.passValue then
                                case setup.damage of
                                    Fixed val ->
                                       (damageDice, currentDamage + val)

                                    Roll val ->
                                        (Die val 0 Nothing :: damageDice, currentDamage)
                            else
                                (damageDice, currentDamage)
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
                        ( nextSeed, rollVal, _ ) =
                            rollDie seed currentRoll
                                |> applyModifier currentRoll (MaybeMod currentRoll.modifier)
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
