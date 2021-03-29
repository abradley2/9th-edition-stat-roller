module App exposing (..)

import Json.Decode as D
import Browser exposing (element)

import Main exposing (init, layout, update, Msg(..), Model)

main : Program D.Value Model Msg
main =
    element
        { update = \msg model -> update msg model
        , init = init
        , subscriptions = \_ -> Sub.none
        , view = layout
        }
