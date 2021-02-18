module DropdownMenu exposing (..)

import Accessibility.Role exposing (listBox, listItem, menu, menuItem)
import Accessibility.Widget exposing (hasMenuPopUp)
import Browser.Dom exposing (Error, focus)
import Html as H
import Html.Attributes as A
import Html.Events as E
import Json.Decode as D
import Maybe.Extra exposing (isJust, isNothing)
import Task


init : Model
init =
    False


update : Msg a -> Model -> ( Model, Cmd (Msg a) )
update msg model =
    case msg of
        ElementFocused _ ->
            ( model, Cmd.none )

        ToggleMenuOpen isOpen ->
            ( isOpen, Cmd.none )

        ItemSelected _ menuId ->
            ( False
            , Task.attempt
                ElementFocused
                (focus menuId)
            )


type Msg a
    = ToggleMenuOpen Bool
    | ItemSelected ( a, String ) String
    | ElementFocused (Result Error ())


type alias Model =
    Bool


type alias Config a =
    { selectedLabel : Maybe String
    , label : String
    , items : List ( a, String )
    , id : String
    }


view : Model -> Config a -> H.Html (Msg a)
view model config =
    H.div
        []
        [ H.button
            [ A.classList
                [ ( "black-80 b--light-blue bg-black helvetica button-reset flex justify-between", True )
                , ( "pointer outline-0 pa2 shadow-1 br-0 bt-0 bl bb fw1 f5 h2 f6", True )
                , ( "light-blue w-100", isJust config.selectedLabel )
                , ( "white-50 w4", isNothing config.selectedLabel )
                ]
            , hasMenuPopUp
            , E.onClick (ToggleMenuOpen (not model))
            , A.id config.id
            ]
            [ H.span
                []
                [ H.text <|
                    Maybe.withDefault "" config.selectedLabel
                ]
            , H.i
                [ A.class "fas fa-caret-down pl2" ]
                []
            ]
        , H.div
            [ A.class "relative"
            ]
            [ if model then
                H.node "focus-menu"
                    [ A.class <|
                        "flex flex-column absolute mt1 left-0 right-0 "
                            ++ "absolute bg-black ph2 pv1 br3 ba b--light-blue"
                            ++ " z-1 "
                    , A.tabindex 0
                    , menu
                    , listBox
                    , E.on "requestedclose" (D.succeed <| ToggleMenuOpen False)
                    , A.attribute "show" "true"
                    ]
                    (config.items
                        |> List.map (buttonView config)
                    )

              else
                H.text ""
            , H.label
                [ A.for config.id
                , A.class "f7 white-80 db pl2 pt2"
                ]
                [ H.text config.label ]
            ]
        ]


buttonView : Config a -> ( a, String ) -> H.Html (Msg a)
buttonView config ( value, label ) =
    H.button
        [ menuItem
        , listItem
        , A.class <|
            "bg-white-10 light-blue ba pv2 ph1 br2 normal f7 b--light-blue mv1 helvetica "
                ++ "pointer hover-bg-white-20"
        , E.onClick <| ItemSelected ( value, label ) config.id
        ]
        [ H.text label ]
