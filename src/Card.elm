module Card exposing (..)

import Clan exposing (Clan)
import Element exposing (Element)
import Format exposing (Format)
import Numerical exposing (Numerical)



-- UNIQUE


type Uniqueness
    = Unique
    | NonUnique


uniqueTxt : String
uniqueTxt =
    "⁕"



-- ROLES


type RoleType
    = AirRole
    | WaterRole
    | FireRole
    | VoidRole
    | EarthRole
    | KeeperRole
    | SeekerRole


type ProvinceElement
    = Single Element
    | Double Element Element
    | Tomoe



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


shiroNishiyama : StrongholdProps
shiroNishiyama =
    { id = "shiro-nishiyama"
    , title = "Shiro Nishiyama"
    , clan = Clan.Crab
    , traits = [ "castle" ]
    , bonusStrength = Numerical.FixedModifier 3
    , startingHonor = 10
    , fateValue = 7
    , influenceValue = 10
    , abilities = [ "action: during a conflict, bow this stronghold - each defending character you control gets +1[conflict-military] and +1[conflict-political] until the end of the conflict." ]
    , formatRequirement = Nothing
    , image = "http://lcg-cdn.fantasyflightgames.com/l5r/L5C01_1.jpg"
    , cycle = "core"
    , cardNumber = "1"
    , artist = "Alayna Lemmer"
    }


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
    , traits : List RoleType
    , abilities : List String
    , formatRequirement : Maybe Format
    , image : String
    , cycle : String
    , cardNumber : String
    , artist : String
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
    , image : String
    , cycle : String
    , cardNumber : String
    , artist : String
    }


type alias ProvinceProps =
    { id : String
    , title : String
    , uniqueness : Uniqueness
    , clan : Clan
    , traits : List String
    , strength : Numerical
    , element : ProvinceElement
    , abilities : List String
    , roleRequirement : Maybe RoleType
    , formatRequirement : Maybe Format
    , image : String
    , cycle : String
    , cardNumber : String
    , artist : String
    }


type alias DynastyEventProps =
    { id : String
    , title : String
    , clan : Clan
    , traits : List String
    , cost : Numerical
    , abilities : List String
    , roleRequirement : Maybe RoleType
    , formatRequirement : Maybe Format
    , image : String
    , cycle : String
    , cardNumber : String
    , artist : String
    }


type alias ConflictEventProps =
    { id : String
    , title : String
    , clan : Clan
    , traits : List String
    , cost : Numerical
    , abilities : List String
    , influenceCost : Maybe Int
    , roleRequirement : Maybe RoleType
    , formatRequirement : Maybe Format
    , image : String
    , cycle : String
    , cardNumber : String
    , artist : String
    }


type alias HoldingProps =
    { id : String
    , title : String
    , uniqueness : Uniqueness
    , clan : Clan
    , traits : List String
    , bonusStrength : Numerical
    , abilities : List String
    , roleRequirement : Maybe RoleType
    , formatRequirement : Maybe Format
    , image : String
    , cycle : String
    , cardNumber : String
    , artist : String
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
    , roleRequirement : Maybe RoleType
    , formatRequirement : Maybe Format
    , image : String
    , cycle : String
    , cardNumber : String
    , artist : String
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
    , roleRequirement : Maybe RoleType
    , formatRequirement : Maybe Format
    , image : String
    , cycle : String
    , cardNumber : String
    , artist : String
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
    , roleRequirement : Maybe RoleType
    , formatRequirement : Maybe Format
    , image : String
    , cycle : String
    , cardNumber : String
    , artist : String
    }



-- FUNCTIONS


title : Card -> String
title card =
    let
        x props =
            props.title
                ++ (if isUnique card then
                        " ⁕"

                    else
                        ""
                   )
    in
    case card of
        RoleType (Role props) ->
            x props

        StrongholdType (Stronghold props) ->
            x props

        AttachmentType (Attachment props) ->
            x props

        CharacterType (ConflictCharacter props) ->
            x props

        CharacterType (DynastyCharacter props) ->
            x props

        EventType (ConflictEvent props) ->
            x props

        EventType (DynastyEvent props) ->
            x props

        HoldingType (Holding props) ->
            x props

        ProvinceType (Province props) ->
            x props


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


isConflict : Card -> Bool
isConflict card =
    case card of
        AttachmentType (Attachment _) ->
            True

        CharacterType (ConflictCharacter _) ->
            True

        EventType (ConflictEvent _) ->
            True

        _ ->
            False


isDynasty : Card -> Bool
isDynasty card =
    case card of
        CharacterType (DynastyCharacter _) ->
            True

        EventType (DynastyEvent _) ->
            True

        HoldingType (Holding _) ->
            True

        _ ->
            False


isPlayable : Clan -> Maybe RoleProps -> Card -> Bool
isPlayable deckClan role card =
    let
        allowedByClan x =
            x.clan == deckClan || x.clan == Clan.Neutral

        allowedByRole x =
            case ( x.roleRequirement, role ) of
                ( Nothing, _ ) ->
                    True

                ( Just _, Nothing ) ->
                    False

                ( Just req, Just { traits } ) ->
                    List.member req traits

        splashable { influenceCost } =
            Maybe.map (always True) influenceCost
                |> Maybe.withDefault False
    in
    case card of
        RoleType _ ->
            True

        StrongholdType (Stronghold props) ->
            allowedByClan props

        CharacterType (DynastyCharacter props) ->
            allowedByRole props && allowedByClan props

        EventType (DynastyEvent props) ->
            allowedByRole props && allowedByClan props

        HoldingType (Holding props) ->
            allowedByRole props && allowedByClan props

        ProvinceType (Province props) ->
            allowedByRole props && allowedByClan props

        AttachmentType (Attachment props) ->
            allowedByRole props && (allowedByClan props || splashable props)

        CharacterType (ConflictCharacter props) ->
            allowedByRole props && (allowedByClan props || splashable props)

        EventType (ConflictEvent props) ->
            allowedByRole props && (allowedByClan props || splashable props)


isCharacter : Card -> Bool
isCharacter card =
    case card of
        CharacterType _ ->
            True

        _ ->
            False


isAttachment : Card -> Bool
isAttachment card =
    case card of
        AttachmentType _ ->
            True

        _ ->
            False


isEvent : Card -> Bool
isEvent card =
    case card of
        EventType _ ->
            True

        _ ->
            False


isHolding : Card -> Bool
isHolding card =
    case card of
        HoldingType _ ->
            True

        _ ->
            False
