module API.DB exposing (DeckDoc, saveDeck)

import Clan exposing (Clan)
import Deck
import Dict
import Firestore
import Firestore.Config as Config
import Firestore.Decode as FSDecode
import Firestore.Encode as FSEncode
import Format exposing (Format)
import Shared exposing (CardCollection)
import Task


type alias DeckDoc =
    { authorId : String
    , clan : Clan
    , description : Maybe String
    , format : Format
    , list : Deck.Decklist
    , name : Maybe String
    }



-- fetchDeck : String -> (Result Firestore.Error (Firestore.Document Deck) -> msg) -> Cmd msg
-- fetchDeck deckId msg =
--     firestore
--         |> Firestore.root
--         |> Firestore.collection "decks"
--         |> Firestore.document deckId
--         |> Firestore.get decoder
--         |> Task.attempt msg


saveDeck : (Result Firestore.Error Deck.Deck -> msg) -> CardCollection -> Deck.Deck -> Cmd msg
saveDeck msg cardCollection deck =
    firestore
        |> Firestore.root
        |> Firestore.collection "decks"
        |> Firestore.insert decoder (encoder deck)
        |> Task.map (wrapUp cardCollection)
        |> Task.andThen unwrapDeck
        |> Task.attempt msg


unwrapDeck : Maybe Deck.Deck -> Task.Task Firestore.Error Deck.Deck
unwrapDeck maybeDeck =
    case maybeDeck of
        Nothing ->
            Task.fail
                (Firestore.Response
                    { code = 0
                    , message = "Oops"
                    , status = "BadData"
                    }
                )

        Just x ->
            Task.succeed x


wrapUp : CardCollection -> Firestore.Document DeckDoc -> Maybe Deck.Deck
wrapUp cardCollection doc =
    Deck.fromDecklist cardCollection doc.fields.list
        |> Maybe.map
            (\deckCards ->
                { meta =
                    { authorId = doc.fields.authorId
                    , clan = doc.fields.clan
                    , description = doc.fields.description
                    , format = doc.fields.format
                    , id = Just <| Firestore.id doc.name
                    , name = doc.fields.name
                    }
                , cards = deckCards
                }
            )


encoder : Deck.Deck -> FSEncode.Encoder
encoder deck =
    FSEncode.document
        [ ( "authorId", FSEncode.string deck.meta.authorId )
        , ( "format", FSEncode.string <| Format.toString deck.meta.format )
        , ( "name", FSEncode.maybe FSEncode.string deck.meta.name )
        , ( "description", FSEncode.maybe FSEncode.string deck.meta.description )
        , ( "list", FSEncode.dict FSEncode.int <| Dict.fromList <| Deck.toDecklist deck.cards )
        ]


decoder : FSDecode.Decoder DeckDoc
decoder =
    FSDecode.document DeckDoc
        |> FSDecode.required "authorId" FSDecode.string
        |> FSDecode.required "clan" decodeClan
        |> FSDecode.optional "description" (FSDecode.maybe FSDecode.string) Nothing
        |> FSDecode.required "format" decodeFormat
        |> FSDecode.required "list" decodeDecklist
        |> FSDecode.optional "name" (FSDecode.maybe FSDecode.string) Nothing


decodeDecklist : FSDecode.Field Deck.Decklist
decodeDecklist =
    FSDecode.dict FSDecode.int |> FSDecode.map Dict.toList


decodeClan : FSDecode.Field Clan
decodeClan =
    FSDecode.string
        |> FSDecode.andThen
            (\str ->
                case Clan.fromString str of
                    Just clan ->
                        FSDecode.succeed clan

                    Nothing ->
                        FSDecode.fail "Unrecognized Clan"
            )


decodeFormat : FSDecode.Field Format
decodeFormat =
    FSDecode.string
        |> FSDecode.andThen
            (\str ->
                case Format.fromString str of
                    Just format ->
                        FSDecode.succeed format

                    Nothing ->
                        FSDecode.fail "Unrecognized Format"
            )


firestore : Firestore.Firestore
firestore =
    Config.new
        { apiKey = "AIzaSyCu2fEiCuFiI-bOCFx-J02E6DwziM9HtBg"
        , project = "miya-library"
        }
        |> Firestore.init
