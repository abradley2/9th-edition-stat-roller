module Presets exposing (..)

import Accessibility as H
import Die exposing (Modifier)
import Fields exposing (Fields)
import Run


type alias Model a =
    { a
        | fields : Fields
        , saveModifier : Maybe Modifier
        , woundModifier : Maybe Modifier
        , weaponSkillModifier : Maybe Modifier
    }


presetsForm : (Model a -> msg) -> Model a -> H.Html msg
presetsForm onNewModel model =
    H.div [] []


attackerPresets : List ( String, Model a -> Model a )
attackerPresets =
    [ ( "Craftworld Guardians"
      , Fields.weaponSkillValue.set (Just 3)
            >> Fields.strengthValue.set (Just 4)
            >> Fields.damageValue.set (Just <| Run.Fixed 1)
            >> (\model -> { model | woundModifier = Just shurikenWeapons })
      )
    ]


defenderPresets : List ( String, Model a -> Model a )
defenderPresets =
    [ ( "Plague Marines"
      , Fields.toughnessValue.set (Just 4)
            >> Fields.saveValue.set (Just 3)
            >> (\model -> { model | saveModifier = Just <| disgustinglyReslient 3 })
      )
    ]


shurikenWeapons : Modifier
shurikenWeapons =
    Die.Compare Die.Eq
        6
        (Die.InfluenceNext <|
            Die.Compare Die.Always 0 (Die.SubtractValue 3)
        )


disgustinglyReslient : Int -> Modifier
disgustinglyReslient save =
    Die.Compare Die.Lte
        (save - 1)
        (Die.InfluenceNext <|
            Die.Compare Die.Always 0 (Die.Reroll (Just 5))
        )


weaponSkillPresets : List ( String, Modifier )
weaponSkillPresets =
    []


woundPresets : List Modifier
woundPresets =
    [-- add one for shuriken weapons
    ]
