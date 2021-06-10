module UI.Decklist exposing (Model, view)

import Card
import Clan exposing (Clan(..))
import Deck exposing (Deck)
import Html exposing (..)
import Html.Attributes exposing (..)
import Tuple
import UI.Icon


type alias DecklistEntry c =
    ( c, Int )


type alias Model =
    { name : Maybe String
    , author : String
    , deck : Deck
    }


view : Model -> Html msg
view model =
    let
        { name, author, deck } =
            model

        { attachments, stronghold, role, conflictCharacters, conflictEvents, dynastyCharacters, dynastyEvents, provinces, holdings } =
            deck

        influence =
            influenceData stronghold.clan attachments conflictCharacters conflictEvents
    in
    div [ class "decklist" ]
        [ div [ class "decklist-stronghold" ]
            [ img
                [ src <| "/assets/card-" ++ stronghold.id ++ ".webp"
                , attribute "loading" "lazy"
                ]
                []
            ]
        , section [ class "decklist-header" ]
            [ h1 [ class "decklist-title" ] [ text <| Maybe.withDefault "Unnamed" name ]
            , p [ class "decklist-byline" ] [ text <| "By " ++ author ]
            , h2 [ class "decklist-synth" ]
                [ strong [] [ text <| stronghold.title ]
                , text " - "
                , text <| Maybe.withDefault "" <| Maybe.map .title role
                ]
            , p []
                ([ strong [] [ text "Influence:" ]
                 , text <| String.fromInt <| List.foldl (\( _, ( _, clanTotal ) ) total -> total + clanTotal) 0 influence
                 , text " of "
                 , text <|
                    String.fromInt <|
                        case role of
                            Just { traits } ->
                                if List.member Card.KeeperRole traits then
                                    stronghold.influenceValue + 3

                                else
                                    stronghold.influenceValue

                            _ ->
                                stronghold.influenceValue
                 , text " - "
                 ]
                    ++ List.map (\( clan, ( cardCount, _ ) ) -> span [] [ text <| String.fromInt cardCount, text " ", UI.Icon.small <| UI.Icon.clan clan ]) influence
                )
            , ul [ class "decklist-provincelist" ] <| List.map provinceEntries provinces
            ]
        , section [ class "decklist-dynasty_deck" ]
            (h3 []
                [ text "Dynasty Deck ("
                , text <| String.fromInt <| sumCards dynastyCharacters + sumCards holdings + sumCards dynastyEvents
                , text ")"
                ]
                :: cardBlock "Characters" dynastyCharacters
                ++ cardBlock "Events" dynastyEvents
                ++ cardBlock "Holdings" holdings
            )
        , section [ class "decklist-conflict_deck" ]
            (h3 []
                [ text "Conflict Deck ("
                , text <| String.fromInt <| sumCards attachments + sumCards conflictCharacters + sumCards conflictEvents
                , text ")"
                ]
                :: cardBlock "Attachments" attachments
                ++ cardBlock "Characters" conflictCharacters
                ++ cardBlock "Events" conflictEvents
            )
        ]


provinceEntries : Card.ProvinceProps -> Html msg
provinceEntries province =
    li [ class "decklist-province" ]
        ([ text province.title, text " " ]
            ++ (case province.element of
                    Card.Single el ->
                        [ UI.Icon.small <| UI.Icon.element el ]

                    Card.Double el1 el2 ->
                        [ UI.Icon.small <| UI.Icon.element el1, UI.Icon.small <| UI.Icon.element el2 ]

                    Card.Tomoe ->
                        [ UI.Icon.small UI.Icon.Fiverings ]
               )
        )


cardBlock : String -> List (DecklistEntry { t | title : String }) -> List (Html msg)
cardBlock title cards =
    let
        cardRow ( card, n ) =
            if n < 1 then
                Nothing

            else
                Just (li [ class "decklist-cardrow" ] [ text (String.fromInt n ++ "x " ++ card.title) ])
    in
    if sumCards cards > 0 then
        [ h4 [] [ text <| title ++ " (" ++ String.fromInt (sumCards cards) ++ ")" ]
        , ul [ class "decklist-cardlist" ] <| List.filterMap cardRow cards
        ]

    else
        []


sumCards : List (DecklistEntry a) -> Int
sumCards =
    List.sum << List.map Tuple.second


influenceData : Clan -> List (DecklistEntry Card.AttachmentProps) -> List (DecklistEntry Card.ConflictCharacterProps) -> List (DecklistEntry Card.ConflictEventProps) -> List ( Clan, ( Int, Int ) )
influenceData deckClan attachments characters events =
    let
        addTuples ( xa, xb ) ( ya, yb ) =
            ( xa + ya, xb + yb )

        init =
            { crab = ( 0, 0 ), crane = ( 0, 0 ), dragon = ( 0, 0 ), lion = ( 0, 0 ), phoenix = ( 0, 0 ), scorpion = ( 0, 0 ), unicorn = ( 0, 0 ) }

        influenceStatsByClan ( card, n ) grouped =
            case ( card.clan, card.influenceCost ) of
                ( _, Nothing ) ->
                    grouped

                ( Neutral, _ ) ->
                    grouped

                ( Shadowlands, _ ) ->
                    grouped

                ( Crab, Just influence ) ->
                    { grouped | crab = Tuple.mapBoth ((+) n) ((+) (n * influence)) grouped.crab }

                ( Crane, Just influence ) ->
                    { grouped | crane = Tuple.mapBoth ((+) n) ((+) (n * influence)) grouped.crane }

                ( Dragon, Just influence ) ->
                    { grouped | dragon = Tuple.mapBoth ((+) n) ((+) (n * influence)) grouped.dragon }

                ( Lion, Just influence ) ->
                    { grouped | lion = Tuple.mapBoth ((+) n) ((+) (n * influence)) grouped.lion }

                ( Phoenix, Just influence ) ->
                    { grouped | phoenix = Tuple.mapBoth ((+) n) ((+) (n * influence)) grouped.phoenix }

                ( Scorpion, Just influence ) ->
                    { grouped | scorpion = Tuple.mapBoth ((+) n) ((+) (n * influence)) grouped.scorpion }

                ( Unicorn, Just influence ) ->
                    { grouped | unicorn = Tuple.mapBoth ((+) n) ((+) (n * influence)) grouped.unicorn }
    in
    [ List.foldl influenceStatsByClan init attachments
    , List.foldl influenceStatsByClan init characters
    , List.foldl influenceStatsByClan init events
    ]
        |> List.foldl
            (\a b ->
                { b
                    | crab = addTuples a.crab b.crab
                    , crane = addTuples a.crane b.crane
                    , dragon = addTuples a.dragon b.dragon
                    , lion = addTuples a.lion b.lion
                    , phoenix = addTuples a.phoenix b.phoenix
                    , scorpion = addTuples a.scorpion b.scorpion
                    , unicorn = addTuples a.unicorn b.unicorn
                }
            )
            init
        |> (\b ->
                [ ( Crab, b.crab )
                , ( Crane, b.crane )
                , ( Dragon, b.dragon )
                , ( Lion, b.lion )
                , ( Phoenix, b.phoenix )
                , ( Scorpion, b.scorpion )
                , ( Unicorn, b.unicorn )
                ]
           )
        |> List.filter (\( clan, ( cardCount, _ ) ) -> clan /= deckClan && cardCount > 0)
