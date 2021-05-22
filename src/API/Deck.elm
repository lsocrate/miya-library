module API.Deck exposing (FetchResult, fetchDeck)

import Http
import Json.Decode exposing (Decoder, field, int, keyValuePairs)


type alias Decklist =
    List ( String, Int )


type alias FetchResult =
    Result Http.Error Decklist


fetchDeck : (FetchResult -> msg) -> String -> Cmd msg
fetchDeck msg deckId =
    Http.get
        { url = "https://api.fiveringsdb.com/decks/" ++ deckId
        , expect = Http.expectJson msg deckDecoder
        }


deckDecoder : Decoder Decklist
deckDecoder =
    field "record" (field "cards" (keyValuePairs int))
