module Rules.Cards exposing (..)

import Rules.Clans exposing (Clan(..))
import Rules.Elements
import Rules.Formats as Formats
import Rules.Roles as Roles


type CardBack
    = Setup
    | Dynasty
    | Conflict


type Card
    = Character CardBack CharacterData
    | Holding CardBack HoldingData
    | Event CardBack EventData
    | Attachment CardBack AttachmentData
    | Stronghold CardBack StrongholdData
    | Role CardBack RoleData
    | Province CardBack ProvinceData


hasBack : CardBack -> Card -> Bool
hasBack targetBackType card =
    case card of
        Character cardBack _ ->
            cardBack == targetBackType

        Holding cardBack _ ->
            cardBack == targetBackType

        Event cardBack _ ->
            cardBack == targetBackType

        Attachment cardBack _ ->
            cardBack == targetBackType

        Stronghold cardBack _ ->
            cardBack == targetBackType

        Role cardBack _ ->
            cardBack == targetBackType

        Province cardBack _ ->
            cardBack == targetBackType


type Uniqueness
    = Unique
    | NonUnique


type alias CharacterData =
    { title : String
    , uniqueness : Uniqueness
    , clan : Clan
    , deck : CardBack
    , traits : List String
    , cost : Maybe Int
    , militarySkill : Maybe Int
    , politicalSkill : Maybe Int
    , glory : Int
    , abilities : List String
    , influenceCost : Maybe Int
    , roleRequirement : Maybe Roles.Trait
    , formatRequirement : Maybe Formats.Format
    , cycle : String
    , cardNumber : Int
    , artist : String
    }


type alias HoldingData =
    { title : String
    , uniqueness : Uniqueness
    , clan : Clan
    , deck : CardBack
    , traits : List String
    , bonusStrength : Int
    , abilities : List String
    , roleRequirement : Maybe Roles.Trait
    , formatRequirement : Maybe Formats.Format
    , cycle : String
    , cardNumber : Int
    , artist : String
    }


type alias AttachmentData =
    { title : String
    , uniqueness : Uniqueness
    , clan : Clan
    , deck : CardBack
    , traits : List String
    , cost : Maybe Int
    , militarySkillBonus : Maybe Int
    , politicalSkillBonus : Maybe Int
    , abilities : List String
    , influenceCost : Maybe Int
    , roleRequirement : Maybe Roles.Trait
    , formatRequirement : Maybe Formats.Format
    , cycle : String
    , cardNumber : Int
    , artist : String
    }


type alias ProvinceData =
    { title : String
    , uniqueness : Uniqueness
    , clan : Clan
    , traits : List String
    , strength : Int
    , elements : List Rules.Elements.Element
    , abilities : List String
    , roleRequirement : Maybe Roles.Trait
    , formatRequirement : Maybe Formats.Format
    , cycle : String
    , cardNumber : Int
    , artist : String
    }


type alias EventData =
    { title : String
    , clan : Clan
    , deck : CardBack
    , traits : List String
    , cost : Maybe Int
    , abilities : List String
    , influenceCost : Maybe Int
    , roleRequirement : Maybe Roles.Trait
    , formatRequirement : Maybe Formats.Format
    , cycle : String
    , cardNumber : Int
    , artist : String
    }


type alias StrongholdData =
    { title : String
    , clan : Clan
    , traits : List String
    , strength : Int
    , startingHonor : Int
    , fateValue : Int
    , influenceValue : Int
    , abilities : List String
    , formatRequirement : Maybe Formats.Format
    , cycle : String
    , cardNumber : Int
    , artist : String
    }


type alias RoleData =
    { title : String
    , traits : List Roles.Trait
    , abilities : List String
    , formatRequirement : Maybe Formats.Format
    , cycle : String
    , cardNumber : Int
    , artist : String
    }


sample : { format : Formats.Format, stronghold : StrongholdData, role : RoleData, provinces : List ProvinceData, deckCards : List ( Card, Int ) }
sample =
    { format = Formats.Stronghold Formats.Jade
    , stronghold =
        StrongholdData "Shiro Shinjo"
            Rules.Clans.Unicorn
            [ "Castle" ]
            1
            10
            6
            10
            [ "More money" ]
            Nothing
            "Cycle"
            123
            "Bob the Painter"
    , role =
        RoleData "Seeker of Void"
            [ Roles.Seeker, Roles.Void ]
            [ "More influence" ]
            Nothing
            "Cycle"
            33
            "Bob the Painter"
    , provinces =
        [ ProvinceData "Massing at Twilight"
            NonUnique
            Rules.Clans.Neutral
            []
            8
            [ Rules.Elements.Void ]
            [ "Weird math" ]
            (Just Roles.Void)
            Nothing
            "Cycle X"
            12
            "Jane the painter"
        , ProvinceData "Border Fortress"
            NonUnique
            Rules.Clans.Unicorn
            [ "Fort" ]
            4
            [ Rules.Elements.Air ]
            [ "Flip provs" ]
            Nothing
            Nothing
            "Cycle X"
            1
            "Jane the painter"
        ]
    , deckCards =
        [ ( Character Dynasty
                (CharacterData "Merchant of Curiosities"
                    NonUnique
                    Rules.Clans.Unicorn
                    Dynasty
                    [ "Courtier", "Merchant" ]
                    (Just 1)
                    (Just 1)
                    (Just 1)
                    1
                    []
                    Nothing
                    Nothing
                    Nothing
                    "Cycle A"
                    123
                    "Bob"
                )
          , 3
          )
        , ( Holding Dynasty
                (HoldingData "Reserve Tents"
                    NonUnique
                    Rules.Clans.Unicorn
                    Dynasty
                    [ "Battlefield" ]
                    2
                    [ "Move people" ]
                    Nothing
                    Nothing
                    "Cycle B"
                    123
                    "Bob"
                )
          , 2
          )
        , ( Event Dynasty
                (EventData "Ride at Dawn"
                    Rules.Clans.Unicorn
                    Dynasty
                    []
                    (Just 0)
                    [ "Rally", "Discard cards" ]
                    Nothing
                    Nothing
                    Nothing
                    "Cycle B"
                    123
                    "Bob"
                )
          , 1
          )
        , ( Attachment Conflict
                (AttachmentData "Curved Blade"
                    NonUnique
                    Rules.Clans.Unicorn
                    Conflict
                    [ "Gaijin", "Weapon" ]
                    (Just 0)
                    (Just 1)
                    (Just 0)
                    [ "Bonus on attack" ]
                    (Just 1)
                    Nothing
                    Nothing
                    "Cycle x"
                    123
                    "Anna"
                )
          , 3
          )
        , ( Character Conflict
                (CharacterData "Merchant of Curiosities"
                    Unique
                    Rules.Clans.Unicorn
                    Conflict
                    [ "Courtier", "Merchant" ]
                    (Just 2)
                    (Just 1)
                    (Just 2)
                    1
                    [ "Some province stuff" ]
                    (Just 2)
                    Nothing
                    Nothing
                    "Cycle A"
                    99
                    "Bob"
                )
          , 2
          )
        , ( Event Conflict
                (EventData "Voice of Honor"
                    Rules.Clans.Crane
                    Conflict
                    []
                    (Just 0)
                    [ "Cancel" ]
                    (Just 2)
                    Nothing
                    Nothing
                    "Cycle B"
                    12
                    "Bob"
                )
          , 1
          )
        ]
    }
