module UI.Filters exposing (Model, init, isClanFilteredOut, isConflictFilteredOut, isDynastyFilteredOut, view)

import Clan exposing (Clan(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck)
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


isClanFilteredOut : Model -> Clan.Clan -> Bool
isClanFilteredOut model clan =
    (model.byClan /= noClanFilter)
        && not (isActiveClan model clan)


isDynastyFilteredOut : Model -> Bool
isDynastyFilteredOut model =
    (model.byBack /= noBackFilter)
        && not (isBackActive model Dynasty)


isConflictFilteredOut : Model -> Bool
isConflictFilteredOut model =
    (model.byBack /= noBackFilter)
        && not (isBackActive model Conflict)



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
