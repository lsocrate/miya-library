module UI.Filters exposing
    ( Model
    , init
    , isAttachmentFilteredOut
    , isCharacterFilteredOut
    , isClanFilteredOut
    , isConflictFilteredOut
    , isDynastyFilteredOut
    , isEventFilteredOut
    , isHoldingFilteredOut
    , view
    )

import Clan exposing (Clan(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck)
import UI.Icon as Icon


type alias Model =
    { byClan : ClanFilter
    , byBack : BackFilter
    , byCardType : CardTypeFilter
    }


type alias ClanFilter =
    { crab : Bool
    , crane : Bool
    , dragon : Bool
    , lion : Bool
    , phoenix : Bool
    , scorpion : Bool
    , unicorn : Bool
    , neutral : Bool
    , shadowlands : Bool
    }


noClanFilter : ClanFilter
noClanFilter =
    { crab = False
    , crane = False
    , dragon = False
    , lion = False
    , phoenix = False
    , scorpion = False
    , unicorn = False
    , neutral = False
    , shadowlands = False
    }


type alias BackFilter =
    { conflict : Bool, dynasty : Bool }


noBackFilter : BackFilter
noBackFilter =
    { conflict = False, dynasty = False }


type Back
    = Conflict
    | Dynasty


type alias CardTypeFilter =
    { character : Bool
    , attachment : Bool
    , event : Bool
    , holding : Bool
    }


noCardTypeFilter : CardTypeFilter
noCardTypeFilter =
    { character = False
    , attachment = False
    , event = False
    , holding = False
    }


type CardType
    = Character
    | Attachment
    | Event
    | Holding


init : Model
init =
    { byClan = noClanFilter, byBack = noBackFilter, byCardType = noCardTypeFilter }


isActiveClan : Model -> Clan -> Bool
isActiveClan { byClan } clan =
    case clan of
        Crab ->
            byClan.crab

        Crane ->
            byClan.crane

        Dragon ->
            byClan.dragon

        Lion ->
            byClan.lion

        Phoenix ->
            byClan.phoenix

        Scorpion ->
            byClan.scorpion

        Unicorn ->
            byClan.unicorn

        Neutral ->
            byClan.neutral

        Shadowlands ->
            byClan.shadowlands


isBackActive : Model -> Back -> Bool
isBackActive { byBack } back =
    case back of
        Conflict ->
            byBack.conflict

        Dynasty ->
            byBack.dynasty


isCardTypeActive : Model -> CardType -> Bool
isCardTypeActive { byCardType } cardType =
    case cardType of
        Character ->
            byCardType.character

        Attachment ->
            byCardType.attachment

        Event ->
            byCardType.event

        Holding ->
            byCardType.holding


toggleClan : Model -> Clan -> Bool -> Model
toggleClan model clan value =
    let
        { byClan } =
            model

        newByClan =
            case clan of
                Crab ->
                    { byClan | crab = value }

                Crane ->
                    { byClan | crane = value }

                Dragon ->
                    { byClan | dragon = value }

                Lion ->
                    { byClan | lion = value }

                Phoenix ->
                    { byClan | phoenix = value }

                Scorpion ->
                    { byClan | scorpion = value }

                Unicorn ->
                    { byClan | unicorn = value }

                Neutral ->
                    { byClan | neutral = value }

                Shadowlands ->
                    { byClan | shadowlands = value }
    in
    { model | byClan = newByClan }


toggleBack : Model -> Back -> Bool -> Model
toggleBack model back value =
    let
        { byBack } =
            model

        newByBack =
            case back of
                Conflict ->
                    { byBack | conflict = value }

                Dynasty ->
                    { byBack | dynasty = value }
    in
    { model | byBack = newByBack }


toggleCardType : Model -> CardType -> Bool -> Model
toggleCardType model cardType value =
    let
        { byCardType } =
            model

        newByCardType =
            case cardType of
                Character ->
                    { byCardType | character = value }

                Attachment ->
                    { byCardType | attachment = value }

                Event ->
                    { byCardType | event = value }

                Holding ->
                    { byCardType | holding = value }
    in
    { model | byCardType = newByCardType }


isClanFilteredOut : Model -> Clan.Clan -> Bool
isClanFilteredOut model clan =
    (model.byClan /= noClanFilter)
        && not (isActiveClan model clan)


isCardBackFilteredOut : Back -> Model -> Bool
isCardBackFilteredOut back model =
    (model.byBack /= noBackFilter)
        && not (isBackActive model back)


isDynastyFilteredOut : Model -> Bool
isDynastyFilteredOut =
    isCardBackFilteredOut Dynasty


isConflictFilteredOut : Model -> Bool
isConflictFilteredOut =
    isCardBackFilteredOut Conflict


isCardTypeFilteredOut : CardType -> Model -> Bool
isCardTypeFilteredOut cardType model =
    (model.byCardType /= noCardTypeFilter)
        && not (isCardTypeActive model cardType)


isCharacterFilteredOut : Model -> Bool
isCharacterFilteredOut =
    isCardTypeFilteredOut Character


isAttachmentFilteredOut : Model -> Bool
isAttachmentFilteredOut =
    isCardTypeFilteredOut Attachment


isEventFilteredOut : Model -> Bool
isEventFilteredOut =
    isCardTypeFilteredOut Event


isHoldingFilteredOut : Model -> Bool
isHoldingFilteredOut =
    isCardTypeFilteredOut Holding



-- VIEW


view : Model -> (Model -> msg) -> List (Html msg)
view model changeMsg =
    let
        clanToggle clan =
            label
                [ classList
                    [ ( "filterblock-item", True )
                    , ( "filterblock-item--active", isActiveClan model clan )
                    ]
                ]
                [ Icon.medium <| Icon.clan clan
                , input
                    [ type_ "checkbox"
                    , onCheck (changeMsg << toggleClan model clan)
                    ]
                    []
                ]

        cardBackToggle cardBack =
            label
                [ classList
                    [ ( "filterblock-item", True )
                    , ( "filterblock-item--conflict", cardBack == Conflict )
                    , ( "filterblock-item--dynasty", cardBack == Dynasty )
                    , ( "filterblock-item--active", isBackActive model cardBack )
                    ]
                ]
                [ div
                    [ class "filterblock-icon" ]
                    [ Icon.medium Icon.Fiverings ]
                , input
                    [ type_ "checkbox"
                    , onCheck (changeMsg << toggleBack model cardBack)
                    ]
                    []
                ]

        cardTypeToggle cardType =
            label
                [ classList
                    [ ( "filterblock-item", True )

                    -- , ( "filterblock-item--conflict", cardType == Conflict ) , ( "filterblock-item--dynasty", cardType == Dynasty )
                    , ( "filterblock-item--active", isCardTypeActive model cardType )
                    ]
                ]
                [ div
                    [ class "filterblock-icon" ]
                    [ Icon.medium
                        (case cardType of
                            Character ->
                                Icon.Character

                            Attachment ->
                                Icon.Attachment

                            Event ->
                                Icon.Event

                            Holding ->
                                Icon.Holding
                        )
                    ]
                , input
                    [ type_ "checkbox"
                    , onCheck (changeMsg << toggleCardType model cardType)
                    ]
                    []
                ]
    in
    [ div [ class "filterblock", class "filterblock--clans" ] <|
        List.map clanToggle
            [ Crab
            , Crane
            , Dragon
            , Lion
            , Phoenix
            , Scorpion
            , Unicorn
            , Neutral
            , Shadowlands
            ]
    , div [ class "filterblock", class "filterblock--backs" ] <|
        List.map cardBackToggle
            [ Dynasty, Conflict ]
    , div [ class "filterblock", class "filterblock--types" ] <|
        List.map cardTypeToggle
            [ Character, Attachment, Event, Holding ]
    ]
