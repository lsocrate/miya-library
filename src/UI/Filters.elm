module UI.Filters exposing (..)

import Card exposing (Card)
import Clan exposing (Clan(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick)
import UI.Icon as Icon


type alias Model =
    { byClan : ClanFilter
    , byBack : BackFilter
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


init : Model
init =
    { byClan = noClanFilter, byBack = noBackFilter }


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


compositeFilter : Model -> Card -> Bool
compositeFilter model card =
    let
        isClanOk =
            (model.byClan == noClanFilter)
                || (isActiveClan model <| Card.clan card)

        isBackOk =
            (model.byBack == noBackFilter)
                || (case card of
                        Card.AttachmentType (Card.Attachment _) ->
                            isBackActive model Conflict

                        Card.CharacterType (Card.ConflictCharacter _) ->
                            isBackActive model Conflict

                        Card.EventType (Card.ConflictEvent _) ->
                            isBackActive model Conflict

                        Card.EventType (Card.DynastyEvent _) ->
                            isBackActive model Dynasty

                        Card.HoldingType (Card.Holding _) ->
                            isBackActive model Dynasty

                        Card.CharacterType (Card.DynastyCharacter _) ->
                            isBackActive model Dynasty

                        _ ->
                            False
                   )
    in
    isClanOk && isBackOk



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
                [ img
                    [ class "filterblock-icon"
                    , src <| Clan.mon clan
                    , alt <| Clan.name clan
                    ]
                    []
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
                    [ class "filterblock-icon"
                    ]
                    [ Icon.icon Icon.Medium Icon.Fiverings ]
                , input
                    [ type_ "checkbox"
                    , onCheck (changeMsg << toggleBack model cardBack)
                    ]
                    []
                ]
    in
    [ div [ class "filterblock" ] <| List.map clanToggle clanOptions
    , div [ class "filterblock" ] <| List.map cardBackToggle cardBackOptions
    ]


clanOptions : List Clan
clanOptions =
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


cardBackOptions : List Back
cardBackOptions =
    [ Dynasty, Conflict ]
