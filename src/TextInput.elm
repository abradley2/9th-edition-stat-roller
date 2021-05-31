module TextInput exposing (..)

import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as D
import Maybe.Extra


view : List (H.Attribute a) -> Maybe String -> Maybe String -> (String -> a) -> H.Html a
view attrs rawValue decoratedValue onInput =
    H.node
        "text-input"
        ([ E.on "newval" (D.map onInput <| D.at [ "detail" ] D.string)
         , A.attribute "input-class" <|
            "input-reset ba bg-black light-blue hover-bn helvetica border-box "
                ++ " br-0 bt-0 outline-0 pa2 h2 tc b--light-blue f6"
         , A.attribute "raw-value" (Maybe.withDefault "" rawValue)
         , A.attribute "decorated-value"
            (Maybe.withDefault "" <|
                Maybe.Extra.or decoratedValue rawValue
            )
         ]
            ++ attrs
        )
        []
