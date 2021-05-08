module Pages.Deckbuilder exposing (Model, Msg, page)

import Components.Header
import Gen.Params.Deckbuilder exposing (Params)
import Html exposing (..)
import Html.Attributes exposing (..)
import Page
import Request
import Rules.Cards as Cards
import Rules.Formats as Formats
import Shared
import String
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { format : Formats.Format
    , stronghold : Cards.StrongholdData
    , role : Maybe Cards.RoleData
    , provinces : List Cards.ProvinceData
    , deckCards : List ( Cards.Card, Int )
    , deckName : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( { format = Cards.sample.format
      , stronghold = Cards.sample.stronghold
      , role = Just Cards.sample.role
      , provinces = Cards.sample.provinces
      , deckCards = Cards.sample.deckCards
      , deckName = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ReplaceMe


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Deckbuilder"
    , body =
        [ Components.Header.view
        , viewDeck model
        ]
    }


viewDeck : Model -> Html Msg
viewDeck model =
    let
        ( dynastyDeck, other ) =
            List.partition (Tuple.first >> Cards.hasBack Cards.Dynasty) model.deckCards

        ( conflictDeck, _ ) =
            List.partition (Tuple.first >> Cards.hasBack Cards.Conflict) other
    in
    main_ [ class "decklist", id "decklist" ]
        [ div [ class "decklist-deck_name" ] (viewDeckName model)
        , div [ class "decklist-header" ] (viewDeckHeader model)
        , div [ class "decklist-decks" ]
            [ div [ class "decklist-deck" ] (viewDeckSide dynastyDeck)
            , div [ class "decklist-deck" ] (viewDeckSide conflictDeck)
            ]
        ]


viewDeckSide : List ( Cards.Card, Int ) -> List (Html Msg)
viewDeckSide cards =
    let
        line x card =
            Just (li [] [ text (String.fromInt x), text card.title ])

        viewCard ( card, x ) =
            case card of
                Cards.Character _ character ->
                    line x character

                Cards.Event _ event ->
                    line x event

                Cards.Holding _ holding ->
                    line x holding

                Cards.Attachment _ attachment ->
                    line x attachment

                _ ->
                    Nothing
    in
    [ ul [] (List.filterMap viewCard cards) ]


viewDeckName : Model -> List (Html Msg)
viewDeckName model =
    case model.deckName of
        Just deckName ->
            [ h1 [ class "decklist-deck_name" ] [ text deckName ] ]

        Nothing ->
            [ h1 [ class "decklist-deck_name" ] [ text "Unnamed" ] ]


viewDeckHeader : Model -> List (Html Msg)
viewDeckHeader model =
    [ div [ class "decklist-header_stronghold" ]
        [ text model.stronghold.title ]
    , div [ class "decklist-header_details" ]
        [ h2 [] [ text model.stronghold.title ]
        , h3 [] (viewRole model)
        , ul [] (viewProvinces model)
        ]
    ]


viewRole : Model -> List (Html Msg)
viewRole model =
    case model.role of
        Just role ->
            [ text role.title ]

        Nothing ->
            [ text "" ]


viewProvinces : Model -> List (Html Msg)
viewProvinces model =
    let
        viewProvince province =
            li [] [ text province.title ]
    in
    List.map viewProvince model.provinces
