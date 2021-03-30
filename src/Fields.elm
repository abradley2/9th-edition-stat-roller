module Fields exposing (..)

import Monocle.Compose as Compose
import Monocle.Lens exposing (..)
import Run exposing (FixedOrRoll(..))


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
    , damage : Field FixedOrRoll
    , toughness : Field Int
    , save : Field Int
    }


type alias Model a =
    { a | fields : Fields }


init : Fields
init =
    { weaponSkill = Field "weapon-skill" "Weapon/Ballistic Skill" Nothing Nothing
    , unitCount = Field "unit-count" "Attacking Units" Nothing Nothing
    , attackCount = Field "attack count" "Attacks per Unit" Nothing Nothing
    , strength = Field "strength" "Attack Strength" Nothing Nothing
    , armorPenetration = Field "armor-penetration" "Armor Penetration" Nothing Nothing
    , damage = Field "damage" "Damage" Nothing Nothing
    , toughness = Field "toughness" "Toughness" Nothing Nothing
    , save = Field "save" "Save" Nothing Nothing
    }


initArmorPenetration : Int -> Field Int
initArmorPenetration value =
    Field "armor-penetration" "Armor Penetration" (Just value) Nothing


fields : Lens (Model b) Fields
fields =
    Lens .fields (\b a -> { a | fields = b })


fieldValue : Lens (Field a) (Maybe a)
fieldValue =
    Lens .value (\b a -> { a | value = b })


fieldError : Lens (Field a) (Maybe String)
fieldError =
    Lens .error (\b a -> { a | error = b })


weaponSkill : Lens (Model b) (Field Int)
weaponSkill =
    Compose.lensWithLens
        (Lens .weaponSkill (\b a -> { a | weaponSkill = b }))
        fields


weaponSkillValue : Lens (Model b) (Maybe Int)
weaponSkillValue =
    Compose.lensWithLens fieldValue weaponSkill


weaponSkillError : Lens (Model b) (Maybe String)
weaponSkillError =
    Compose.lensWithLens fieldError weaponSkill


unitCount : Lens (Model b) (Field Int)
unitCount =
    Compose.lensWithLens
        (Lens .unitCount (\b a -> { a | unitCount = b }))
        fields


unitCountValue : Lens (Model b) (Maybe Int)
unitCountValue =
    Compose.lensWithLens fieldValue unitCount


unitCountError : Lens (Model b) (Maybe String)
unitCountError =
    Compose.lensWithLens fieldError unitCount


attackCount : Lens (Model b) (Field Int)
attackCount =
    Compose.lensWithLens
        (Lens .attackCount (\b a -> { a | attackCount = b }))
        fields


attackCountValue : Lens (Model b) (Maybe Int)
attackCountValue =
    Compose.lensWithLens fieldValue attackCount


attackCountError : Lens (Model b) (Maybe String)
attackCountError =
    Compose.lensWithLens fieldError attackCount


strength : Lens (Model b) (Field Int)
strength =
    Compose.lensWithLens
        (Lens .strength (\b a -> { a | strength = b }))
        fields


strengthValue : Lens (Model b) (Maybe Int)
strengthValue =
    Compose.lensWithLens fieldValue strength


strengthError : Lens (Model b) (Maybe String)
strengthError =
    Compose.lensWithLens fieldError strength


armorPenetration : Lens (Model b) (Field Int)
armorPenetration =
    Compose.lensWithLens
        (Lens .armorPenetration (\b a -> { a | armorPenetration = b }))
        fields


armorPenetrationValue : Lens (Model b) (Maybe Int)
armorPenetrationValue =
    Compose.lensWithLens fieldValue armorPenetration


armorPenetrationError : Lens (Model b) (Maybe String)
armorPenetrationError =
    Compose.lensWithLens fieldError armorPenetration


toughness : Lens (Model b) (Field Int)
toughness =
    Compose.lensWithLens
        (Lens .toughness (\b a -> { a | toughness = b }))
        fields


toughnessValue : Lens (Model b) (Maybe Int)
toughnessValue =
    Compose.lensWithLens fieldValue toughness


toughnessError : Lens (Model b) (Maybe String)
toughnessError =
    Compose.lensWithLens fieldError toughness


damage : Lens (Model b) (Field FixedOrRoll)
damage =
    Compose.lensWithLens
        (Lens .damage (\b a -> { a | damage = b }))
        fields


damageValue : Lens (Model b) (Maybe FixedOrRoll)
damageValue =
    Compose.lensWithLens fieldValue damage


damageError : Lens (Model b) (Maybe String)
damageError =
    Compose.lensWithLens fieldError damage


save : Lens (Model b) (Field Int)
save =
    Compose.lensWithLens
        (Lens .save (\b a -> { a | save = b }))
        fields


saveValue : Lens (Model b) (Maybe Int)
saveValue =
    Compose.lensWithLens fieldValue save


saveError : Lens (Model b) (Maybe String)
saveError =
    Compose.lensWithLens fieldError save
