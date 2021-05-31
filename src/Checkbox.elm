module Checkbox exposing (..)

import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as D


type alias Config a =
    { label : String
    , id : String
    , onToggle : Bool -> a
    , isChecked : Bool
    , class : Maybe String
    }


checkboxView : Config a -> H.Html a
checkboxView config =
    H.div
        []
        [ H.label
            [ A.for config.id
            , A.class <| "inline-flex pointer items-stretch " ++ Maybe.withDefault "" config.class
            , A.tabindex 0
            , E.on "keydown"
                (D.andThen
                    (\code ->
                        if code == 13 then
                            D.succeed (config.onToggle <| not config.isChecked)

                        else
                            D.fail ""
                    )
                    E.keyCode
                )
            ]
            [ H.i
                [ A.classList
                    [ ( "fas f3 light-blue", True )
                    , ( "fa-square", not config.isChecked )
                    , ( "fa-check-square", config.isChecked )
                    ]
                ]
                []
            , H.span
                [ A.classList
                    [ ( "nowrap pl2 f7 self-center ", True )
                    , ( "white-50", not config.isChecked )
                    ]
                ]
                [ H.text config.label
                ]
            ]
        , H.input
            [ A.id config.id
            , A.class "dn"
            , A.tabindex -1
            , A.type_ "checkbox"
            , A.checked config.isChecked
            , E.onCheck config.onToggle
            ]
            []
        ]
