module Cards exposing (..)

-- UNIQUE


type Uniqueness
    = Unique
    | NonUnique



-- CLANS


type Clan
    = Crab
    | Crane
    | Dragon
    | Lion
    | Phoenix
    | Scorpion
    | Unicorn
    | Neutral


clanName : Clan -> String
clanName clan =
    case clan of
        Crab ->
            "Crab"

        Crane ->
            "Crane"

        Dragon ->
            "Dragon"

        Lion ->
            "Lion"

        Phoenix ->
            "Phoenix"

        Scorpion ->
            "Scorpion"

        Unicorn ->
            "Unicorn"

        Neutral ->
            "Neutral"



-- ROLES


type RoleTypes
    = AirRole
    | WaterRole
    | FireRole
    | VoidRole
    | EarthRole
    | KeeperRole
    | SeekerRole



-- FORMATS


type Format
    = Stronghold RL
    | Skirmish
    | Enlightenment
    | Draft


type RL
    = Imperial
    | Jade



-- ELEMENTS


type Element
    = Air
    | Water
    | Fire
    | Void
    | Earth


type alias RoleProperties =
    { title : String
    , traits : List RoleTypes
    , abilities : List String
    , formatRequirement : Maybe Format
    , cycle : String
    , cardNumber : Int
    , artist : String
    }


type alias StrongholdProperties =
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


type alias ProvinceProperties =
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



-- CARDS


type Card
    = RoleCard RoleProperties
    | StrongholdCard StrongholdProperties
    | ProvinceCard ProvinceProperties
    | DynastyHolding
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
    | DynastyEvent
        { title : String
        , clan : Clan
        , traits : List String
        , cost : Maybe Int
        , abilities : List String
        , roleRequirement : Maybe RoleTypes
        , formatRequirement : Maybe Format
        , cycle : String
        , cardNumber : Int
        , artist : String
        }
    | ConflictEvent
        { title : String
        , clan : Clan
        , traits : List String
        , cost : Maybe Int
        , abilities : List String
        , influenceCost : Maybe Int
        , roleRequirement : Maybe RoleTypes
        , formatRequirement : Maybe Format
        , cycle : String
        , cardNumber : Int
        , artist : String
        }
    | DynastyCharacter
        { title : String
        , uniqueness : Uniqueness
        , clan : Clan
        , traits : List String
        , cost : Maybe Int
        , militarySkill : Maybe Int
        , politicalSkill : Maybe Int
        , glory : Int
        , abilities : List String
        , roleRequirement : Maybe RoleTypes
        , formatRequirement : Maybe Format
        , cycle : String
        , cardNumber : Int
        , artist : String
        }
    | ConflictCharacter
        { title : String
        , uniqueness : Uniqueness
        , clan : Clan
        , traits : List String
        , cost : Maybe Int
        , militarySkill : Maybe Int
        , politicalSkill : Maybe Int
        , glory : Int
        , abilities : List String
        , influenceCost : Maybe Int
        , roleRequirement : Maybe RoleTypes
        , formatRequirement : Maybe Format
        , cycle : String
        , cardNumber : Int
        , artist : String
        }
    | ConflictAttachment
        { title : String
        , uniqueness : Uniqueness
        , clan : Clan
        , traits : List String
        , cost : Maybe Int
        , militarySkillBonus : Maybe Int
        , politicalSkillBonus : Maybe Int
        , abilities : List String
        , influenceCost : Maybe Int
        , roleRequirement : Maybe RoleTypes
        , formatRequirement : Maybe Format
        , cycle : String
        , cardNumber : Int
        , artist : String
        }
