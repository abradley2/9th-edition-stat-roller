module TextInput exposing (..)

import Html as H
import Html.Attributes as A
import Html.Events as E

view : List (H.Attribute a) -> (String -> a) -> H.Html a
view attrs onInput =
    H.input
        ([ E.onInput onInput
         , A.spellcheck False
         , A.autocomplete False
         , A.class <|
            "input-reset ba bg-black light-blue hover-bn helvetica "
                ++ " br-0 bt-0 outline-0 ph2 pv2 tc b--light-blue"
         ]
            ++ attrs
        )
        []
