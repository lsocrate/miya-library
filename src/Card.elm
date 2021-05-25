module Card exposing (..)

import Clan exposing (Clan)
import Format exposing (Format)
import Numerical exposing (Numerical)



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
    | Earth
    | Fire
    | Void
    | Water


elementName : Element -> String
elementName element =
    case element of
        Air ->
            "Air"

        Earth ->
            "Earth"

        Fire ->
            "Fire"

        Void ->
            "Void"

        Water ->
            "Water"



-- CARDS


type Card
    = AttachmentType Attachment
    | CharacterType Character
    | EventType Event
    | HoldingType Holding
    | ProvinceType Province
    | RoleType Role
    | StrongholdType Stronghold


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
    { id : String
    , title : String
    , traits : List RoleTypes
    , abilities : List String
    , formatRequirement : Maybe Format
    , image : Maybe String
    , cycle : Maybe String
    , cardNumber : Maybe String
    , artist : Maybe String
    }


type alias StrongholdProps =
    { id : String
    , title : String
    , clan : Clan
    , traits : List String
    , bonusStrength : Numerical
    , startingHonor : Int
    , fateValue : Int
    , influenceValue : Int
    , abilities : List String
    , formatRequirement : Maybe Format
    , image : Maybe String
    , cycle : Maybe String
    , cardNumber : Maybe String
    , artist : Maybe String
    }


type alias ProvinceProps =
    { id : String
    , title : String
    , uniqueness : Uniqueness
    , clan : Clan
    , traits : List String
    , strength : Numerical
    , elements : List Element
    , abilities : List String
    , roleRequirement : Maybe RoleTypes
    , formatRequirement : Maybe Format
    , image : Maybe String
    , cycle : Maybe String
    , cardNumber : Maybe String
    , artist : Maybe String
    }


type alias DynastyEventProps =
    { id : String
    , title : String
    , clan : Clan
    , traits : List String
    , cost : Numerical
    , abilities : List String
    , roleRequirement : Maybe RoleTypes
    , formatRequirement : Maybe Format
    , image : Maybe String
    , cycle : Maybe String
    , cardNumber : Maybe String
    , artist : Maybe String
    }


type alias ConflictEventProps =
    { id : String
    , title : String
    , clan : Clan
    , traits : List String
    , cost : Numerical
    , abilities : List String
    , influenceCost : Maybe Int
    , roleRequirement : Maybe RoleTypes
    , formatRequirement : Maybe Format
    , image : Maybe String
    , cycle : Maybe String
    , cardNumber : Maybe String
    , artist : Maybe String
    }


type alias HoldingProps =
    { id : String
    , title : String
    , uniqueness : Uniqueness
    , clan : Clan
    , traits : List String
    , bonusStrength : Numerical
    , abilities : List String
    , roleRequirement : Maybe RoleTypes
    , formatRequirement : Maybe Format
    , image : Maybe String
    , cycle : Maybe String
    , cardNumber : Maybe String
    , artist : Maybe String
    }


type alias AttachmentProps =
    { id : String
    , title : String
    , uniqueness : Uniqueness
    , clan : Clan
    , traits : List String
    , cost : Numerical
    , militarySkillBonus : Numerical
    , politicalSkillBonus : Numerical
    , abilities : List String
    , influenceCost : Maybe Int
    , roleRequirement : Maybe RoleTypes
    , formatRequirement : Maybe Format
    , image : Maybe String
    , cycle : Maybe String
    , cardNumber : Maybe String
    , artist : Maybe String
    }


type alias DynastyCharacterProps =
    { id : String
    , title : String
    , uniqueness : Uniqueness
    , clan : Clan
    , traits : List String
    , cost : Numerical
    , militarySkill : Numerical
    , politicalSkill : Numerical
    , glory : Int
    , abilities : List String
    , roleRequirement : Maybe RoleTypes
    , formatRequirement : Maybe Format
    , image : Maybe String
    , cycle : Maybe String
    , cardNumber : Maybe String
    , artist : Maybe String
    }


type alias ConflictCharacterProps =
    { id : String
    , title : String
    , uniqueness : Uniqueness
    , clan : Clan
    , traits : List String
    , cost : Numerical
    , militarySkill : Numerical
    , politicalSkill : Numerical
    , glory : Int
    , abilities : List String
    , influenceCost : Maybe Int
    , roleRequirement : Maybe RoleTypes
    , formatRequirement : Maybe Format
    , image : Maybe String
    , cycle : Maybe String
    , cardNumber : Maybe String
    , artist : Maybe String
    }



-- FUNCTIONS


title : Card -> String
title card =
    case card of
        RoleType (Role props) ->
            props.title

        StrongholdType (Stronghold props) ->
            props.title

        AttachmentType (Attachment props) ->
            props.title

        CharacterType (ConflictCharacter props) ->
            props.title

        CharacterType (DynastyCharacter props) ->
            props.title

        EventType (ConflictEvent props) ->
            props.title

        EventType (DynastyEvent props) ->
            props.title

        HoldingType (Holding props) ->
            props.title

        ProvinceType (Province props) ->
            props.title


clan : Card -> Clan.Clan
clan card =
    case card of
        RoleType _ ->
            Clan.Neutral

        StrongholdType (Stronghold props) ->
            props.clan

        AttachmentType (Attachment props) ->
            props.clan

        CharacterType (ConflictCharacter props) ->
            props.clan

        CharacterType (DynastyCharacter props) ->
            props.clan

        EventType (ConflictEvent props) ->
            props.clan

        EventType (DynastyEvent props) ->
            props.clan

        HoldingType (Holding props) ->
            props.clan

        ProvinceType (Province props) ->
            props.clan


id : Card -> String
id card =
    case card of
        RoleType (Role props) ->
            props.id

        StrongholdType (Stronghold props) ->
            props.id

        AttachmentType (Attachment props) ->
            props.id

        CharacterType (ConflictCharacter props) ->
            props.id

        CharacterType (DynastyCharacter props) ->
            props.id

        EventType (ConflictEvent props) ->
            props.id

        EventType (DynastyEvent props) ->
            props.id

        HoldingType (Holding props) ->
            props.id

        ProvinceType (Province props) ->
            props.id


cost : Card -> Maybe Numerical
cost card =
    case card of
        AttachmentType (Attachment props) ->
            Just props.cost

        CharacterType (ConflictCharacter props) ->
            Just props.cost

        CharacterType (DynastyCharacter props) ->
            Just props.cost

        EventType (ConflictEvent props) ->
            Just props.cost

        EventType (DynastyEvent props) ->
            Just props.cost

        _ ->
            Nothing


military : Card -> Maybe Numerical
military card =
    case card of
        AttachmentType (Attachment props) ->
            Just props.militarySkillBonus

        CharacterType (ConflictCharacter props) ->
            Just props.militarySkill

        CharacterType (DynastyCharacter props) ->
            Just props.militarySkill

        _ ->
            Nothing


political : Card -> Maybe Numerical
political card =
    case card of
        AttachmentType (Attachment props) ->
            Just props.politicalSkillBonus

        CharacterType (ConflictCharacter props) ->
            Just props.politicalSkill

        CharacterType (DynastyCharacter props) ->
            Just props.politicalSkill

        _ ->
            Nothing


glory : Card -> Maybe Int
glory card =
    case card of
        CharacterType (ConflictCharacter props) ->
            Just props.glory

        CharacterType (DynastyCharacter props) ->
            Just props.glory

        _ ->
            Nothing


strength : Card -> Maybe Numerical
strength card =
    case card of
        StrongholdType (Stronghold props) ->
            Just props.bonusStrength

        HoldingType (Holding props) ->
            Just props.bonusStrength

        ProvinceType (Province props) ->
            Just props.strength

        _ ->
            Nothing


influence : Card -> Maybe Int
influence card =
    case card of
        CharacterType (ConflictCharacter props) ->
            props.influenceCost

        AttachmentType (Attachment props) ->
            props.influenceCost

        EventType (ConflictEvent props) ->
            props.influenceCost

        _ ->
            Nothing


isUnique : Card -> Bool
isUnique card =
    let
        unique props =
            case props.uniqueness of
                Unique ->
                    True

                NonUnique ->
                    False
    in
    case card of
        AttachmentType (Attachment props) ->
            unique props

        CharacterType (ConflictCharacter props) ->
            unique props

        CharacterType (DynastyCharacter props) ->
            unique props

        HoldingType (Holding props) ->
            unique props

        ProvinceType (Province props) ->
            unique props

        _ ->
            False
