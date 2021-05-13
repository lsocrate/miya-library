module Card exposing (..)

import Clan exposing (Clan)
import Format exposing (Format)



-- UNIQUE


type Uniqueness
    = Unique
    | NonUnique



-- ROLES


type RoleTypes
    = AirRole
    | WaterRole
    | FireRole
    | VoidRole
    | EarthRole
    | KeeperRole
    | SeekerRole



-- ELEMENTS


type Element
    = Air
    | Water
    | Fire
    | Void
    | Earth



-- NUMERICAL


type NumericalValue
    = FixedValue Int
    | DashValue
    | XValue


type NumericalModifier
    = FixedModifier Int
    | XModifier



-- CARDS


type Card
    = AttachmentCard Attachment
    | CharacterCard Character
    | EventCard Event
    | HoldingCard Holding
    | ProvinceCard Province
    | RoleCard Role
    | StrongholdCard Stronghold


type Stronghold
    = Stronghold StrongholdProps


type Role
    = Role RoleProps


type Province
    = Province ProvinceProps


type Attachment
    = Attachment AttachmentProps


type Holding
    = Holding HoldingProps


type Character
    = DynastyCharacter DynastyCharacterProps
    | ConflictCharacter ConflictCharacterProps


type Event
    = DynastyEvent DynastyEventProps
    | ConflictEvent ConflictEventProps


type alias RoleProps =
    { title : String
    , traits : List RoleTypes
    , abilities : List String
    , formatRequirement : Maybe Format
    , cycle : String
    , cardNumber : Int
    , artist : String
    }


type alias StrongholdProps =
    { title : String
    , clan : Clan
    , traits : List String
    , bonusStrength : Int
    , startingHonor : Int
    , fateValue : Int
    , influenceValue : Int
    , abilities : List String
    , formatRequirement : Maybe Format
    , cycle : String
    , cardNumber : Int
    , artist : String
    }


type alias ProvinceProps =
    { title : String
    , uniqueness : Uniqueness
    , clan : Clan
    , traits : List String
    , strength : Int
    , elements : List Element
    , abilities : List String
    , roleRequirement : Maybe RoleTypes
    , formatRequirement : Maybe Format
    , cycle : String
    , cardNumber : Int
    , artist : String
    }


type alias DynastyEventProps =
    { title : String
    , clan : Clan
    , traits : List String
    , cost : NumericalValue
    , abilities : List String
    , roleRequirement : Maybe RoleTypes
    , formatRequirement : Maybe Format
    , cycle : String
    , cardNumber : Int
    , artist : String
    }


type alias ConflictEventProps =
    { title : String
    , clan : Clan
    , traits : List String
    , cost : NumericalValue
    , abilities : List String
    , influenceCost : Maybe Int
    , roleRequirement : Maybe RoleTypes
    , formatRequirement : Maybe Format
    , cycle : String
    , cardNumber : Int
    , artist : String
    }


type alias HoldingProps =
    { title : String
    , uniqueness : Uniqueness
    , clan : Clan
    , traits : List String
    , bonusStrength : Int
    , abilities : List String
    , roleRequirement : Maybe RoleTypes
    , formatRequirement : Maybe Format
    , cycle : String
    , cardNumber : Int
    , artist : String
    }


type alias AttachmentProps =
    { title : String
    , uniqueness : Uniqueness
    , clan : Clan
    , traits : List String
    , cost : NumericalValue
    , militarySkillBonus : NumericalModifier
    , politicalSkillBonus : NumericalModifier
    , abilities : List String
    , influenceCost : Maybe Int
    , roleRequirement : Maybe RoleTypes
    , formatRequirement : Maybe Format
    , cycle : String
    , cardNumber : Int
    , artist : String
    }


type alias DynastyCharacterProps =
    { title : String
    , uniqueness : Uniqueness
    , clan : Clan
    , traits : List String
    , cost : NumericalValue
    , militarySkill : NumericalValue
    , politicalSkill : NumericalValue
    , glory : Int
    , abilities : List String
    , roleRequirement : Maybe RoleTypes
    , formatRequirement : Maybe Format
    , cycle : String
    , cardNumber : Int
    , artist : String
    }


type alias ConflictCharacterProps =
    { title : String
    , uniqueness : Uniqueness
    , clan : Clan
    , traits : List String
    , cost : NumericalValue
    , militarySkill : NumericalValue
    , politicalSkill : NumericalValue
    , glory : Int
    , abilities : List String
    , influenceCost : Maybe Int
    , roleRequirement : Maybe RoleTypes
    , formatRequirement : Maybe Format
    , cycle : String
    , cardNumber : Int
    , artist : String
    }
