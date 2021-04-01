module Die exposing (..)

import Maybe.Extra as MaybeX
import Random


type DieState
    = Failed Int
    | Passed Int
    | NotRolled


type alias Die =
    { sides : Int
    , passValue : Int
    , state : DieState
    , modifier : Maybe Modifier
    , id : Int
    }
extractValue : Die -> Int
extractValue die =
    case die.state of
        Passed val ->
            val

        Failed val ->
            val

        _ ->
            -1


checkPass : Random.Seed -> Die -> ( Random.Seed, Die, Int )
checkPass seed die =
    let
        getState =
            \val ->
                val
                    |> (if val >= die.passValue then
                            Passed

                        else
                            Failed
                       )
    in
    case die.state of
        NotRolled ->
            let
                ( val, nextSeed ) =
                    Random.step (Random.int 1 die.sides) seed

                state =
                    if val >= die.passValue then
                        Passed val

                    else
                        Failed val
            in
            ( nextSeed, { die | state = state }, val )

        Failed val ->
            ( seed, { die | state = getState val }, val )

        Passed val ->
            ( seed, { die | state = getState val }, val )


type Compare
    = Always
    | Lte
    | Gte
    | Eq


type Modifier
    = AddValue Int
    | SubtractValue Int
    | InfluenceNext Modifier
    | Reroll (Maybe Int)
    | Compare Compare Int Modifier
    | MaybeMod (Maybe Modifier)
    | Batch (List Modifier)


rollDie : Die -> Random.Seed -> ( Random.Seed, Die, Maybe Modifier)
rollDie die seed =
    let
        (rollSeed, rolledDie, maybeMod) = rollDie_ die seed

        state =checkPass rollSeed rolledDie
            |> (\(_, d, _) -> d)
            |> .state
            |>  Debug.log "state -> "


    in
        (rollSeed, { rolledDie | state = state }, maybeMod)

rollDie_ : Die -> Random.Seed -> ( Random.Seed, Die, Maybe Modifier )
rollDie_ die seed =
    let
        ( nextSeed, nextDie, dieVal ) =
            checkPass seed die
        modifier =
            die.modifier
                |> Maybe.withDefault (MaybeMod Nothing)
    in
    case modifier of
        MaybeMod mMod ->
            mMod
                |> Maybe.map (\nextMod -> rollDie_ { nextDie | modifier = Just nextMod } nextSeed)
                |> Maybe.withDefault ( nextSeed, nextDie, Nothing )

        Compare Lte val nextMod ->
            if dieVal <= val then
                rollDie_ { nextDie | modifier = Just nextMod } seed

            else
                ( nextSeed, nextDie, Nothing )

        Compare Always _ nextMod ->
            rollDie_ { nextDie | modifier = Just nextMod } seed

        Compare Gte val nextMod ->
            if dieVal >= val then
                rollDie_ { nextDie | modifier = Just nextMod } nextSeed

            else
                ( nextSeed, nextDie, Nothing )

        Compare Eq val nextMod ->
            if dieVal == val then
                rollDie_ { nextDie | modifier = Just nextMod } nextSeed

            else
                ( nextSeed, nextDie, Nothing )

        AddValue plusVal ->
            ( nextSeed, { nextDie | passValue = nextDie.passValue - plusVal }, Nothing )

        SubtractValue minusVal ->
            ( nextSeed, { nextDie | passValue = nextDie.passValue + minusVal }, Nothing )

        Reroll mNewVal ->
            let
                rerollDie =
                    Maybe.map (\passValue -> { nextDie | passValue = passValue }) mNewVal
                        |> Maybe.withDefault nextDie
                        |> (\d -> { d | state = NotRolled })
            in
            rollDie_ rerollDie nextSeed

        InfluenceNext nextMod ->
            ( nextSeed, nextDie, Just nextMod )

        Batch modList ->
            List.foldr
                (\curMod ( curSeed, curDie, curNextMod ) ->
                    let
                        ( nextSeed_, nextDie_, mNextMod_ ) =
                            rollDie_ { curDie | modifier = Just curMod } curSeed
                    in
                    ( nextSeed_, Debug.log "MODIFIED" nextDie_, MaybeX.or mNextMod_ curNextMod )
                )
                ( nextSeed, nextDie, Nothing )
                modList
