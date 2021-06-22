module UI.Decklist exposing (Model, view)

import Card
import Clan exposing (Clan(..))
import Deck exposing (Deck)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Tuple
import UI.Card
import UI.Icon


type alias DecklistEntry c =
    ( c, Int )


type alias Model =
    { name : Maybe String
    , author : String
    , deck : Deck
    , editingName : Bool
    }


type alias Actions msg =
    { startUpdateName : msg
    , updateName : String -> msg
    , doneUpdateName : String -> msg
    }


view : Maybe (Actions msg) -> Model -> Html msg
view actions { name, author, deck, editingName } =
    let
        { attachments, stronghold, role, conflictCharacters, conflictEvents, dynastyCharacters, dynastyEvents, provinces, holdings } =
            deck

        influence =
            influenceData stronghold.clan attachments conflictCharacters conflictEvents

        cardBlockDynasty =
            cardBlock (always [])

        cardBlockConflict =
            cardBlock
                (\{ influenceCost, clan } ->
                    if clan == deck.stronghold.clan then
                        []

                    else
                        [ span
                            [ classList
                                [ ( "decklist-influencemarker", True )
                                , ( "decklist-influencemarker--crab", clan == Crab )
                                , ( "decklist-influencemarker--crane", clan == Crane )
                                , ( "decklist-influencemarker--dragon", clan == Dragon )
                                , ( "decklist-influencemarker--lion", clan == Lion )
                                , ( "decklist-influencemarker--phoenix", clan == Phoenix )
                                , ( "decklist-influencemarker--scorpion", clan == Scorpion )
                                , ( "decklist-influencemarker--unicorn", clan == Unicorn )
                                ]
                            ]
                            (case influenceCost of
                                Just 1 ->
                                    [ text " ", UI.Icon.small UI.Icon.Influence1 ]

                                Just 2 ->
                                    [ text " ", UI.Icon.small UI.Icon.Influence2 ]

                                Just 3 ->
                                    [ text " ", UI.Icon.small UI.Icon.Influence3 ]

                                Just 4 ->
                                    [ text " ", UI.Icon.small UI.Icon.Influence4 ]

                                _ ->
                                    []
                            )
                        ]
                )
    in
    div [ class "decklist" ]
        [ div [ class "decklist-stronghold" ] [ UI.Card.eager stronghold ]
        , section [ class "decklist-header" ]
            [ h1 [ class "decklist-title" ]
                (case ( editingName, actions ) of
                    ( False, Nothing ) ->
                        [ text <| Maybe.withDefault "Unnamed" name ]

                    ( False, Just { startUpdateName } ) ->
                        [ text <| Maybe.withDefault "Unnamed" name
                        , button [ onClick startUpdateName ] [ text "edit" ]
                        ]

                    ( True, Just { updateName, doneUpdateName } ) ->
                        [ input
                            [ onInput updateName
                            , value <| Maybe.withDefault "" name
                            ]
                            []
                        , button [ onClick <| doneUpdateName <| Maybe.withDefault "" name ] [ text "save" ]
                        ]

                    _ ->
                        [ text "" ]
                )
            , p [ class "decklist-byline" ] [ text <| "By " ++ author ]
            , h2 [ class "decklist-synth" ]
                [ strong [] [ text <| stronghold.title ]
                , text " - "
                , text <| Maybe.withDefault "" <| Maybe.map .title role
                ]
            , influenceDescription (Deck.maxInfluence deck) influence
            , ul [ class "decklist-cardlist", class "decklist-cardlist--provinces" ] <| List.map provinceEntries provinces
            ]
        , section [ class "decklist-dynasty_deck" ]
            (List.concat
                [ sideHeader "Dynasty" (sumCards dynastyCharacters + sumCards holdings + sumCards dynastyEvents)
                , cardBlockDynasty "Characters" dynastyCharacters
                , cardBlockDynasty "Events" dynastyEvents
                , cardBlockDynasty "Holdings" holdings
                ]
            )
        , section [ class "decklist-conflict_deck" ]
            (List.concat
                [ sideHeader "Conflict" (sumCards attachments + sumCards conflictCharacters + sumCards conflictEvents)
                , cardBlockConflict "Attachments" attachments
                , cardBlockConflict "Characters" conflictCharacters
                , cardBlockConflict "Events" conflictEvents
                ]
            )
        ]


sideHeader : String -> Int -> List (Html msg)
sideHeader sideName cardCount =
    [ h3 [ class "decklist-side_header" ]
        [ text sideName
        , text " Deck ("
        , text <| String.fromInt cardCount
        , text ")"
        ]
    ]


influenceDescription : Int -> List ( Clan, ( Int, Int ) ) -> Html msg
influenceDescription maxInfluence influence =
    let
        spentInfluence =
            List.foldl (\( _, ( _, clanTotal ) ) total -> total + clanTotal) 0 influence

        influenceIcons =
            List.map
                (\( clan, ( cardCount, _ ) ) ->
                    span
                        [ classList
                            [ ( "decklist-influence_entry", True )
                            , ( "decklist-influence_entry--crab", clan == Crab )
                            , ( "decklist-influence_entry--crane", clan == Crane )
                            , ( "decklist-influence_entry--dragon", clan == Dragon )
                            , ( "decklist-influence_entry--lion", clan == Lion )
                            , ( "decklist-influence_entry--phoenix", clan == Phoenix )
                            , ( "decklist-influence_entry--scorpion", clan == Scorpion )
                            , ( "decklist-influence_entry--unicorn", clan == Unicorn )
                            ]
                        ]
                        [ text <| String.fromInt cardCount
                        , text " "
                        , UI.Icon.small <| UI.Icon.clan clan
                        ]
                )
                influence
    in
    p
        [ classList
            [ ( "decklist-influence", True )
            , ( "decklist-influence--ilegal", spentInfluence > maxInfluence )
            ]
        ]
        ([ strong [] [ text "Influence: " ]
         , span []
            [ text <| String.fromInt spentInfluence ]
         , text " of "
         , text <| String.fromInt maxInfluence
         , text <|
            if List.isEmpty influence then
                ""

            else
                " • "
         ]
            ++ influenceIcons
        )


provinceEntries : Card.ProvinceProps -> Html msg
provinceEntries province =
    let
        line =
            List.concat
                [ [ text province.title ]
                , case province.element of
                    Card.Single el ->
                        [ UI.Icon.small <| UI.Icon.element el ]

                    Card.Double el1 el2 ->
                        [ UI.Icon.small <| UI.Icon.element el1
                        , UI.Icon.small <| UI.Icon.element el2
                        ]

                    Card.Tomoe ->
                        [ UI.Icon.small UI.Icon.Fiverings ]
                ]
                |> List.intersperse (text " ")
    in
    cardEntry [ class "decklist-province" ] line province


type alias AnyCard c =
    { c | title : String, id : String }


cardBlock : (AnyCard c -> List (Html msg)) -> String -> List (DecklistEntry (AnyCard c)) -> List (Html msg)
cardBlock cardInfluenceInfo sectionTitle sectionCards =
    let
        cardRow ( card, n ) =
            if n < 1 then
                Nothing

            else
                Just <|
                    cardEntry [] (decksCardLine cardInfluenceInfo card n) card
    in
    if sumCards sectionCards > 0 then
        [ h4 [ class "decklist-type_header" ]
            [ text <| sectionTitle ++ " (" ++ String.fromInt (sumCards sectionCards) ++ ")" ]
        , ul [ class "decklist-cardlist" ] <| List.filterMap cardRow sectionCards
        ]

    else
        []


cardEntry : List (Attribute msg) -> List (Html msg) -> AnyCard c -> Html msg
cardEntry attrs line card =
    li (class "decklist-cardentry" :: attrs)
        [ div [ class "decklist-cardrow" ] line
        , div [ class "decklist-hoverimage" ] [ UI.Card.lazy card ]
        ]


decksCardLine : (AnyCard c -> List (Html msg)) -> AnyCard c -> Int -> List (Html msg)
decksCardLine cardInfluenceInfo card n =
    [ text <| String.fromInt n
    , text " × "
    , text <| card.title
    ]
        ++ cardInfluenceInfo card


sumCards : List (DecklistEntry a) -> Int
sumCards =
    List.sum << List.map Tuple.second


influenceData : Clan -> List (DecklistEntry Card.AttachmentProps) -> List (DecklistEntry Card.ConflictCharacterProps) -> List (DecklistEntry Card.ConflictEventProps) -> List ( Clan, ( Int, Int ) )
influenceData deckClan attachments characters events =
    let
        addTuples a b =
            Tuple.mapBoth ((+) <| Tuple.first a) ((+) <| Tuple.second a) b

        addInfluence qty inf old =
            Tuple.mapBoth ((+) qty) ((+) (qty * inf)) old

        init =
            { crab = ( 0, 0 ), crane = ( 0, 0 ), dragon = ( 0, 0 ), lion = ( 0, 0 ), phoenix = ( 0, 0 ), scorpion = ( 0, 0 ), unicorn = ( 0, 0 ) }

        influenceStatsByClan ( card, n ) grouped =
            case ( card.clan, card.influenceCost ) of
                ( Crab, Just inf ) ->
                    { grouped | crab = addInfluence n inf grouped.crab }

                ( Crane, Just inf ) ->
                    { grouped | crane = addInfluence n inf grouped.crane }

                ( Dragon, Just inf ) ->
                    { grouped | dragon = addInfluence n inf grouped.dragon }

                ( Lion, Just inf ) ->
                    { grouped | lion = addInfluence n inf grouped.lion }

                ( Phoenix, Just inf ) ->
                    { grouped | phoenix = addInfluence n inf grouped.phoenix }

                ( Scorpion, Just inf ) ->
                    { grouped | scorpion = addInfluence n inf grouped.scorpion }

                ( Unicorn, Just inf ) ->
                    { grouped | unicorn = addInfluence n inf grouped.unicorn }

                ( _, _ ) ->
                    grouped
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
