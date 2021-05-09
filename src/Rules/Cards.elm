module Rules.Cards exposing (..)

import Rules.Clans exposing (Clan(..))
import Rules.Elements
import Rules.Formats as Formats
import Rules.Roles as Roles


type CardBack
    = Setup
    | Dynasty
    | Conflict


type CardType
    = Character
    | Holding
    | Event
    | Attachment
    | Stronghold
    | Role
    | Province


type Card
    = CardCharacter CardBack CharacterData
    | CardHolding CardBack HoldingData
    | CardEvent CardBack EventData
    | CardAttachment CardBack AttachmentData
    | CardStronghold CardBack StrongholdData
    | CardRole CardBack RoleData
    | CardProvince CardBack ProvinceData


hasBack : CardBack -> Card -> Bool
hasBack targetBackType card =
    case card of
        CardCharacter cardBack _ ->
            cardBack == targetBackType

        CardHolding cardBack _ ->
            cardBack == targetBackType

        CardEvent cardBack _ ->
            cardBack == targetBackType

        CardAttachment cardBack _ ->
            cardBack == targetBackType

        CardStronghold cardBack _ ->
            cardBack == targetBackType

        CardRole cardBack _ ->
            cardBack == targetBackType

        CardProvince cardBack _ ->
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
        [ ( CardCharacter Dynasty
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
        , ( CardHolding Dynasty
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
        , ( CardEvent Dynasty
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
        , ( CardAttachment Conflict
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
        , ( CardCharacter Conflict
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
        , ( CardEvent Conflict
                { title = "Voice of Honor"
                , clan = Rules.Clans.Crane
                , deck = Conflict
                , traits = []
                , cost = Just 0
                , abilities = [ "Cancel" ]
                , influenceCost = Just 2
                , roleRequirement = Nothing
                , formatRequirement = Nothing
                , cycle = "Cycle B"
                , cardNumber = 12
                , artist = "Bob"
                }
          , 1
          )
        ]
    }
