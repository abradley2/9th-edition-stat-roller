module ModifierForm exposing (..)

import Html as H
import Html.Attributes as A
import Html.Events as E


type Effect
    = EffCmd (Cmd Msg)
    | EffBatch (List Effect)


fromEffect : Effect -> Cmd Msg
fromEffect eff =
    case eff of
        EffCmd cmd ->
            cmd

        EffBatch effList ->
            effList
                |> List.map fromEffect
                |> Cmd.batch


type alias Model =
    {}


type Msg
    = NoOp


type Config
    = AttackMod
    | WoundMod
    | SaveMod


init : Config -> Model
init config =
    {}


update_ : Msg -> Model -> ( Model, Effect )
update_ msg model =
    ( model, EffCmd Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    update_ msg model |> Tuple.mapSecond fromEffect


view : H.Html Msg
view =
    H.div
        []
        [ H.button
            [ A.class <|
                "black-80 ba b--light-blue bg-light-blue helvetica "
                    ++ " pointer outline-0 br2 pa2 shadow-1 "
            ]
            [ H.span [] [ H.text "Select Modifier" ]
            , H.i
                [ A.class "fas fa-caret-down black-70 pl2" ]
                []
            ]
        ]
