module UI.Decklist exposing (Model, view)

import Card
import Clan exposing (Clan(..))
import Deck exposing (DeckCards)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Influence
import Tuple
import UI.Card
import UI.Icon


type alias DecklistEntry c =
    ( c, Int )


type alias Model =
    { name : Maybe String
    , author : String
    , deck : DeckCards
    , editingName : Bool
    }


type alias Actions msg =
    { startUpdateName : msg
    , updateName : String -> msg
    , doneUpdateName : String -> msg
    , toggleProvinceSelector : msg
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
                            [ class "dcklst-influencemarker"
                            , classList
                                [ ( "dcklst-influencemarker--crab", clan == Crab )
                                , ( "dcklst-influencemarker--crane", clan == Crane )
                                , ( "dcklst-influencemarker--dragon", clan == Dragon )
                                , ( "dcklst-influencemarker--lion", clan == Lion )
                                , ( "dcklst-influencemarker--phoenix", clan == Phoenix )
                                , ( "dcklst-influencemarker--scorpion", clan == Scorpion )
                                , ( "dcklst-influencemarker--unicorn", clan == Unicorn )
                                ]
                            ]
                            [ text " ", UI.Icon.influence UI.Icon.small influenceCost ]
                        ]
                )
    in
    div [ class "dcklst" ]
        [ div [ class "dcklst-stronghold" ] [ UI.Card.eager stronghold ]
        , section [ class "dcklst-header" ]
            (List.concat
                [ [ h1 [ class "dcklst-title" ]
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
                  , p [ class "dcklst-byline" ] [ text <| "By " ++ author ]
                  , h2 [ class "dcklst-synth" ]
                        (List.intersperse
                            (text " - ")
                            (List.concat
                                [ [ strong [] [ text stronghold.title ] ]
                                , role
                                    |> Maybe.map (\c -> [ text c.title ])
                                    |> Maybe.withDefault []
                                ]
                            )
                        )
                  , influenceDescription (Deck.maxInfluence deck) influence
                  , ul [ class "dcklst-cardlist", class "dcklst-cardlist--provinces" ] <| List.map provinceEntries provinces
                  ]
                , actions
                    |> Maybe.map (\act -> [ button [ onClick act.toggleProvinceSelector ] [ text "Edit provinces" ] ])
                    |> Maybe.withDefault []
                ]
            )
        , section [ class "dcklst-dynasty_deck" ]
            (List.concat
                [ sideHeader "Dynasty" (sumCards dynastyCharacters + sumCards holdings + sumCards dynastyEvents)
                , cardBlockDynasty "Characters" dynastyCharacters
                , cardBlockDynasty "Events" dynastyEvents
                , cardBlockDynasty "Holdings" holdings
                ]
            )
        , section [ class "dcklst-conflict_deck" ]
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
    [ h3 [ class "dcklst-side_header" ]
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
                            [ ( "dcklst-influence_entry", True )
                            , ( "dcklst-influence_entry--crab", clan == Crab )
                            , ( "dcklst-influence_entry--crane", clan == Crane )
                            , ( "dcklst-influence_entry--dragon", clan == Dragon )
                            , ( "dcklst-influence_entry--lion", clan == Lion )
                            , ( "dcklst-influence_entry--phoenix", clan == Phoenix )
                            , ( "dcklst-influence_entry--scorpion", clan == Scorpion )
                            , ( "dcklst-influence_entry--unicorn", clan == Unicorn )
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
            [ ( "dcklst-influence", True )
            , ( "dcklst-influence--ilegal", spentInfluence > maxInfluence )
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
    cardEntry [ class "dcklst-province" ] line province


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
        [ h4 [ class "dcklst-type_header" ]
            [ text <| sectionTitle ++ " (" ++ String.fromInt (sumCards sectionCards) ++ ")" ]
        , ul [ class "dcklst-cardlist" ] <| List.filterMap cardRow sectionCards
        ]

    else
        []


cardEntry : List (Attribute msg) -> List (Html msg) -> AnyCard c -> Html msg
cardEntry attrs line card =
    li (class "dcklst-cardentry" :: attrs)
        [ div [ class "dcklst-cardrow" ] line
        , div [ class "dcklst-hoverimage" ] [ UI.Card.lazy card ]
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
                ( Crab, inf ) ->
                    { grouped | crab = addInfluence n (Influence.toInt inf) grouped.crab }

                ( Crane, inf ) ->
                    { grouped | crane = addInfluence n (Influence.toInt inf) grouped.crane }

                ( Dragon, inf ) ->
                    { grouped | dragon = addInfluence n (Influence.toInt inf) grouped.dragon }

                ( Lion, inf ) ->
                    { grouped | lion = addInfluence n (Influence.toInt inf) grouped.lion }

                ( Phoenix, inf ) ->
                    { grouped | phoenix = addInfluence n (Influence.toInt inf) grouped.phoenix }

                ( Scorpion, inf ) ->
                    { grouped | scorpion = addInfluence n (Influence.toInt inf) grouped.scorpion }

                ( Unicorn, inf ) ->
                    { grouped | unicorn = addInfluence n (Influence.toInt inf) grouped.unicorn }

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
