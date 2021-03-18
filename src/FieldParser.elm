module FieldParser exposing (..)

import Basics.Extra exposing (flip)
import Parser exposing ((|.), (|=), DeadEnd, Parser, float, int, map, oneOf, run, spaces, succeed, symbol)
import Run exposing (FixedOrRoll(..))


rollParser : Parser FixedOrRoll
rollParser =
    Parser.succeed Roll
        |. symbol "D"
        |= int


fixedOrRollParser : Parser FixedOrRoll
fixedOrRollParser =
    oneOf
        [ rollParser
        , map Fixed int
        ]


parseFixedOrRoll : String -> Maybe FixedOrRoll
parseFixedOrRoll =
    run fixedOrRollParser >> Result.toMaybe


formatFixedOrRoll : FixedOrRoll -> String
formatFixedOrRoll damage =
    case damage of
        Fixed val ->
            String.fromInt val

        Roll val ->
            "D" ++ String.fromInt val


passValueParser : Parser Int
passValueParser =
    oneOf
        [ int
        , succeed (\v -> v)
            |= int
            |. symbol "+"
        ]


parsePassValue : String -> Maybe Int
parsePassValue =
    run passValueParser >> Result.toMaybe


formatPassValue : Int -> String
formatPassValue =
    String.fromInt >> flip (++) "+"
