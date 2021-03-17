module FieldParser exposing (..)

import Parser exposing (Parser, (|.), (|=), succeed, symbol, float, spaces, oneOf, map, int, DeadEnd, run)
import Run exposing (Damage(..))
import Parser

rollParser : Parser Int
rollParser =
    Parser.succeed always
    |. symbol "D"
    |= int

damageParser : Parser Damage
damageParser = oneOf
    [ map Roll rollParser
    , map Fixed int
    ]

parseDamage : String -> Maybe Damage
parseDamage =
    run damageParser
        |> Result.toMaybe
