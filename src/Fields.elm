module Fields exposing (..)

import Monocle.Compose as Compose
import Monocle.Lens exposing (..)


type alias Field a =
    { id : String
    , label : String
    , value : Maybe a
    , error : Maybe String
    }


type alias Fields =
    { weaponSkill : Field Int
    , unitCount : Field Int
    , attackCount : Field Int
    , strength : Field Int
    , armorPenetration : Field Int
    , damage : Field Int
    , toughness : Field Int
    , save : Field Int
    }


init : Fields
init =
    { weaponSkill = Field weaponSkillId "Weapon/Ballistic Skill" Nothing Nothing
    , unitCount = Field unitCountId "Number of Attacking Units" Nothing Nothing
    , attackCount = Field attackCountId "Attacks per Unit" Nothing Nothing
    , strength = Field strengthId "Attack Strength" Nothing Nothing
    , armorPenetration = Field armorPenetrationId "Armor Penetration" Nothing Nothing
    , damage = Field damageId "Damage" Nothing Nothing
    , toughness = Field toughnessId "Toughness" Nothing Nothing
    , save = Field saveId "Save" Nothing Nothing
    }


fieldLabel : Lens Fields (Field a) -> Lens Fields String
fieldLabel =
    Compose.lensWithLens (Lens .label (\b a -> { a | label = b }))


weaponSkill : Lens Fields (Field Int)
weaponSkill =
    Lens .weaponSkill (\b a -> { a | weaponSkill = b })


weaponSkillId =
    "weapon-skill"


unitCount : Lens Fields (Field Int)
unitCount =
    Lens .unitCount (\b a -> { a | unitCount = b })


unitCountId =
    "unit-count"


attackCount : Lens Fields (Field Int)
attackCount =
    Lens .attackCount (\b a -> { a | attackCount = b })


attackCountId =
    "attack-count"


strength : Lens Fields (Field Int)
strength =
    Lens .strength (\b a -> { a | strength = b })


strengthId =
    "strength"


armorPenetration : Lens Fields (Field Int)
armorPenetration =
    Lens .armorPenetration (\b a -> { a | armorPenetration = b })


armorPenetrationId =
    "armor-penetration"


toughness : Lens Fields (Field Int)
toughness =
    Lens .toughness (\b a -> { a | toughness = b })


toughnessId =
    "toughness"


damage : Lens Fields (Field Int)
damage =
    Lens .damage (\b a -> { a | damage = b })


damageId =
    "damage"


save : Lens Fields (Field Int)
save =
    Lens .save (\b a -> { a | save = b })


saveId =
    "save"
