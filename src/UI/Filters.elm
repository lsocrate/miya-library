module UI.Filters exposing
    ( Model
    , Msg(..)
    , blockedCardBacks
    , blockedCardTypes
    , blockedClans
    , blockedInfluenceCost
    , blockedRestricted
    , blockedUniqueness
    , init
    , update
    , view
    )

import Card exposing (Back(..), CardType(..))
import Clan exposing (Clan(..))
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onCheck)
import Html.Lazy exposing (lazy3)
import Influence exposing (InfluenceCost(..))
import UI.Icon as Icon
import Uniqueness exposing (Uniqueness(..))


type alias Model =
    { byClan : Filter Clan
    , byBack : Filter Back
    , byCardType : Filter CardType
    , byUnicity : Filter Uniqueness
    , byRestricted : Filter Restricted
    , byInfluenceCost : Filter InfluenceCost
    }


type Restricted
    = Restricted


type Msg
    = ChangeClan Clan Bool
    | ChangeCardBack Back Bool
    | ChangeCardType CardType Bool
    | ChangeUnicity Uniqueness Bool
    | ChangeRestricted Restricted Bool
    | ChangeInfluenceCost InfluenceCost Bool


type alias Filter category =
    List ( category, Bool )


noClanFilter : Filter Clan
noClanFilter =
    [ ( Crab, False )
    , ( Crane, False )
    , ( Dragon, False )
    , ( Lion, False )
    , ( Phoenix, False )
    , ( Scorpion, False )
    , ( Unicorn, False )
    , ( Neutral, False )
    , ( Shadowlands, False )
    ]


noBackFilter : Filter Back
noBackFilter =
    [ ( Card.Dynasty, False )
    , ( Card.Conflict, False )
    ]


noCardTypeFilter : Filter CardType
noCardTypeFilter =
    [ ( CardTypeCharacter, False )
    , ( CardTypeAttachment, False )
    , ( CardTypeEvent, False )
    , ( CardTypeHolding, False )
    ]


noUnicityFilter : Filter Uniqueness
noUnicityFilter =
    [ ( Unique, False )
    , ( NonUnique, False )
    ]


noRestrictedFilter : Filter Restricted
noRestrictedFilter =
    [ ( Restricted, False ) ]


noInfluenceCostFilter : Filter InfluenceCost
noInfluenceCostFilter =
    [ ( InfluenceCost1, False )
    , ( InfluenceCost2, False )
    , ( InfluenceCost3, False )
    , ( InfluenceCost4, False )
    ]



-----------------
-- INIT
-----------------


init : Model
init =
    { byClan = noClanFilter
    , byBack = noBackFilter
    , byCardType = noCardTypeFilter
    , byUnicity = noUnicityFilter
    , byRestricted = noRestrictedFilter
    , byInfluenceCost = noInfluenceCostFilter
    }



-----------------
-- UPDATE
-----------------


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeClan clan val ->
            { model | byClan = updateFilter model.byClan ( clan, val ) }

        ChangeCardBack cardBack val ->
            { model | byBack = updateFilter model.byBack ( cardBack, val ) }

        ChangeCardType cardType val ->
            { model | byCardType = updateFilter model.byCardType ( cardType, val ) }

        ChangeUnicity unicity val ->
            { model | byUnicity = updateFilter model.byUnicity ( unicity, val ) }

        ChangeRestricted restricted val ->
            { model | byRestricted = updateFilter model.byRestricted ( restricted, val ) }

        ChangeInfluenceCost influenceCost val ->
            { model | byInfluenceCost = updateFilter model.byInfluenceCost ( influenceCost, val ) }


updateFilter : Filter cat -> ( cat, Bool ) -> Filter cat
updateFilter oldFilter newOption =
    let
        replaceOnMatch oldOption =
            if Tuple.first oldOption == Tuple.first newOption then
                newOption

            else
                oldOption
    in
    List.map replaceOnMatch oldFilter



-----------------
-- RESULTS
-----------------


blockedClans : Model -> List Clan
blockedClans =
    isBlock .byClan noClanFilter


blockedCardBacks : Model -> List Back
blockedCardBacks =
    isBlock .byBack noBackFilter


blockedCardTypes : Model -> List CardType
blockedCardTypes =
    isBlock .byCardType noCardTypeFilter


blockedUniqueness : Model -> List Uniqueness
blockedUniqueness =
    isBlock .byUnicity noUnicityFilter


blockedRestricted : Model -> List Restricted
blockedRestricted =
    isBlock .byRestricted noRestrictedFilter


blockedInfluenceCost : Model -> List InfluenceCost
blockedInfluenceCost =
    isBlock .byInfluenceCost noInfluenceCostFilter


isBlock : (Model -> Filter cat) -> Filter cat -> Model -> List cat
isBlock category emptyFilter model =
    if category model == emptyFilter then
        []

    else
        List.filterMap
            (\( option, isOn ) ->
                if isOn then
                    Nothing

                else
                    Just option
            )
            (category model)



-------
-- VIEW
-------


view : List (Attribute msg) -> (Msg -> msg) -> Model -> Html msg
view =
    let
        viewFilters attrs changeMsg model =
            div (class "fltrblk" :: attrs)
                [ group (class "fltrblk-group--clans") model.byClan (clanToggle changeMsg)
                , group (class "fltrblk-group--backs") model.byBack (cardBackToggle changeMsg)
                , group (class "fltrblk-group--types") model.byCardType (cardTypeToggle changeMsg)
                , group (class "fltrblk-group--unicity") model.byUnicity (uniquenessToggle changeMsg)
                , group (class "fltrblk-group--restricted") model.byRestricted (restrictedToggle changeMsg)
                , group (class "fltrblk-group--influence") model.byInfluenceCost (influenceToggle changeMsg)
                ]
    in
    lazy3 viewFilters


group : Attribute msg -> Filter cat -> (( cat, Bool ) -> Html msg) -> Html msg
group gClass opts viewToggle =
    div [ gClass, class "fltrblk-group" ] (List.map viewToggle opts)


genToggle : (cat -> String) -> (cat -> Html msg) -> (cat -> Bool -> msg) -> ( cat, Bool ) -> Html msg
genToggle toClass toIcon changeMsg ( category, isActive ) =
    label
        [ classList
            [ ( toClass category, True )
            , ( "fltrblk-item", True )
            , ( "fltrblk-item--active", isActive )
            ]
        ]
        [ div [ class "fltrblk-icon" ] [ toIcon category ]
        , input
            [ type_ "checkbox"
            , onCheck (changeMsg category)
            ]
            []
        ]


clanToggle : (Msg -> msg) -> ( Clan, Bool ) -> Html msg
clanToggle changeMsg =
    let
        iconView =
            Icon.clan >> Icon.large

        handler clan isOn =
            changeMsg <| ChangeClan clan isOn
    in
    genToggle ((++) "fltrblk-item--" << Clan.toString) iconView handler


cardBackToggle : (Msg -> msg) -> ( Back, Bool ) -> Html msg
cardBackToggle changeMsg =
    let
        classThing cardBack =
            case cardBack of
                Card.Conflict ->
                    "fltrblk-item--conflict"

                Card.Dynasty ->
                    "fltrblk-item--dynasty"

                _ ->
                    ""

        handler cardBack isOn =
            changeMsg <| ChangeCardBack cardBack isOn
    in
    genToggle classThing (always <| Icon.large Icon.Fiverings) handler


cardTypeToggle : (Msg -> msg) -> ( CardType, Bool ) -> Html msg
cardTypeToggle changeMsg =
    let
        iconThing cardType =
            Icon.large
                (case cardType of
                    CardTypeCharacter ->
                        Icon.Character

                    CardTypeAttachment ->
                        Icon.Attachment

                    CardTypeEvent ->
                        Icon.Event

                    CardTypeHolding ->
                        Icon.Holding

                    _ ->
                        Icon.Unicorn
                )

        handler cardType =
            changeMsg << ChangeCardType cardType
    in
    genToggle (always "fltrblk-item--bland") iconThing handler


uniquenessToggle : (Msg -> msg) -> ( Uniqueness, Bool ) -> Html msg
uniquenessToggle changeMsg =
    let
        iconThing extra =
            Icon.large
                (case extra of
                    Unique ->
                        Icon.Unique

                    NonUnique ->
                        Icon.NonUnique
                )

        handler extra =
            changeMsg << ChangeUnicity extra
    in
    genToggle (always "fltrblk-item--bland") iconThing handler


restrictedToggle : (Msg -> msg) -> ( Restricted, Bool ) -> Html msg
restrictedToggle changeMsg =
    let
        handler extra =
            changeMsg << ChangeRestricted extra
    in
    genToggle (always "fltrblk-item--bland") (always <| Icon.large Icon.Restricted) handler


influenceToggle : (Msg -> msg) -> ( InfluenceCost, Bool ) -> Html msg
influenceToggle changeMsg =
    let
        handler influenceCost =
            changeMsg << ChangeInfluenceCost influenceCost
    in
    genToggle (always "fltrblk-item--bland") (Icon.influence Icon.large) handler
