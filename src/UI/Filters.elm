module UI.Filters exposing
    ( Back(..)
    , CardType(..)
    , Extra(..)
    , Model
    , Msg(..)
    , init
    , isCardBackOut
    , isCardTypeOut
    , isClanOut
    , isExtraOut
    , update
    , view
    )

import Clan exposing (Clan(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck)
import Html.Lazy exposing (lazy3)
import UI.Icon as Icon


type alias Model =
    { byClan : Filter Clan
    , byBack : Filter Back
    , byCardType : Filter CardType
    , byExtra : Filter Extra
    }


type Back
    = Conflict
    | Dynasty


type CardType
    = Character
    | Attachment
    | Event
    | Holding


type Extra
    = Unique
    | NonUnique
    | Restricted
    | Influence1
    | Influence2
    | Influence3
    | Influence4


type Msg
    = ChangeClan Clan Bool
    | ChangeCardBack Back Bool
    | ChangeCardType CardType Bool
    | ChangeExtra Extra Bool


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
    [ ( Dynasty, False )
    , ( Conflict, False )
    ]


noCardTypeFilter : Filter CardType
noCardTypeFilter =
    [ ( Character, False )
    , ( Attachment, False )
    , ( Event, False )
    , ( Holding, False )
    ]


noExtraFilter : Filter Extra
noExtraFilter =
    [ ( Unique, False )
    , ( NonUnique, False )
    , ( Restricted, False )
    , ( Influence1, False )
    , ( Influence2, False )
    , ( Influence3, False )
    , ( Influence4, False )
    ]



-----------------
-- INIT
-----------------


init : Model
init =
    { byClan = noClanFilter, byBack = noBackFilter, byCardType = noCardTypeFilter, byExtra = noExtraFilter }



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

        ChangeExtra extra val ->
            { model | byExtra = updateFilter model.byExtra ( extra, val ) }


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


isClanOut : Model -> Clan.Clan -> Bool
isClanOut =
    isCardOut .byClan noClanFilter


isCardBackOut : Model -> Back -> Bool
isCardBackOut =
    isCardOut .byBack noBackFilter


isCardTypeOut : Model -> CardType -> Bool
isCardTypeOut =
    isCardOut .byCardType noCardTypeFilter


isExtraOut : Model -> Extra -> Bool
isExtraOut =
    isCardOut .byExtra noExtraFilter


isCardOut : (Model -> Filter cat) -> Filter cat -> Model -> cat -> Bool
isCardOut category emptyFilter model option =
    (category model /= emptyFilter)
        && not
            (List.member ( option, True ) (category model))



-------
-- VIEW
-------


view : List (Attribute msg) -> (Msg -> msg) -> Model -> Html msg
view =
    lazy3 view_


view_ : List (Attribute msg) -> (Msg -> msg) -> Model -> Html msg
view_ attrs changeMsg model =
    div (class "fltrblk" :: attrs)
        [ group (class "fltrblk-group--clans") model.byClan (clanToggle changeMsg)
        , group (class "fltrblk-group--backs") model.byBack (cardBackToggle changeMsg)
        , group (class "fltrblk-group--types") model.byCardType (cardTypeToggle changeMsg)
        , group (class "fltrblk-group--extra") model.byExtra (extraToggle changeMsg)
        ]


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
                Conflict ->
                    "fltrblk-item--conflict"

                Dynasty ->
                    "fltrblk-item--dynasty"

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
                    Character ->
                        Icon.Character

                    Attachment ->
                        Icon.Attachment

                    Event ->
                        Icon.Event

                    Holding ->
                        Icon.Holding
                )

        handler cardType =
            changeMsg << ChangeCardType cardType
    in
    genToggle (always "fltrblk-item--bland") iconThing handler


extraToggle : (Msg -> msg) -> ( Extra, Bool ) -> Html msg
extraToggle changeMsg =
    let
        iconThing extra =
            Icon.large
                (case extra of
                    Unique ->
                        Icon.Unique

                    NonUnique ->
                        Icon.NonUnique

                    Restricted ->
                        Icon.Restricted

                    Influence1 ->
                        Icon.Influence1

                    Influence2 ->
                        Icon.Influence2

                    Influence3 ->
                        Icon.Influence3

                    Influence4 ->
                        Icon.Influence4
                )

        handler extra =
            changeMsg << ChangeExtra extra
    in
    genToggle (always "fltrblk-item--bland") iconThing handler
