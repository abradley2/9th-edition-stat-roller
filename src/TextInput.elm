module TextInput exposing (..)

import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as D


view : List (H.Attribute a) -> Maybe String -> (String -> a) -> H.Html a
view attrs value onInput =
    H.input
        ([ E.on "change" (D.map onInput <| D.at [ "target", "value" ] D.string)
         , A.spellcheck False
         , A.autocomplete False
         , A.class <|
            "input-reset ba bg-black light-blue hover-bn helvetica border-box "
                ++ " br-0 bt-0 outline-0 pa2 h2 tc b--light-blue f6"
         , case value of
            Just val ->
                A.value val

            Nothing ->
               A.value ""
         ]
            ++ attrs
        )
        []
