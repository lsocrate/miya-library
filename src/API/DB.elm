module API.DB exposing (Deck, saveDeck)

import Deck
import DeckMeta exposing (DeckMeta)
import Dict exposing (Dict)
import Firestore
import Firestore.Config as Config
import Firestore.Decode as FSDecode
import Firestore.Encode as FSEncode
import Firestore.Types.Reference as Reference
import Format exposing (Format)
import Task


type alias Deck =
    { id : String
    , authorId : String
    , format : Maybe Format
    , name : Maybe String
    , description : Maybe String
    , list : Dict String Int
    }



-- fetchDeck : String -> (Result Firestore.Error (Firestore.Document Deck) -> msg) -> Cmd msg
-- fetchDeck deckId msg =
--     firestore
--         |> Firestore.root
--         |> Firestore.collection "decks"
--         |> Firestore.document deckId
--         |> Firestore.get decoder
--         |> Task.attempt msg


saveDeck : (Result Firestore.Error (Firestore.Document Deck) -> msg) -> DeckMeta -> Deck.Deck -> Cmd msg
saveDeck msg meta decklist =
    firestore
        |> Firestore.root
        |> Firestore.collection "decks"
        |> Firestore.insert decoder (encoder meta decklist)
        |> Task.attempt msg


encoder : DeckMeta -> Deck.Deck -> FSEncode.Encoder
encoder meta decklist =
    FSEncode.document
        [ ( "authorId", FSEncode.string meta.authorId )
        , ( "format", FSEncode.string <| Format.toString meta.format )
        , ( "name", FSEncode.maybe FSEncode.string meta.name )
        , ( "description", FSEncode.maybe FSEncode.string meta.description )
        , ( "list", FSEncode.dict FSEncode.int <| Dict.fromList <| Deck.toDecklist decklist )
        ]


decoder : FSDecode.Decoder Deck
decoder =
    FSDecode.document Deck
        |> FSDecode.required "id" (FSDecode.map Reference.toString FSDecode.reference)
        |> FSDecode.required "authorId" FSDecode.string
        |> FSDecode.required "format" (FSDecode.map Format.fromString FSDecode.string)
        |> FSDecode.optional "name" (FSDecode.maybe FSDecode.string) Nothing
        |> FSDecode.optional "description" (FSDecode.maybe FSDecode.string) Nothing
        |> FSDecode.required "list" (FSDecode.dict FSDecode.int)



-- |> FSDecode.required "name" FSDecode.string
-- |> FSDecode.required "description" FSDecode.string
-- |> FSDecode.required "list" FSDecode.string


firestore : Firestore.Firestore
firestore =
    Config.new
        { apiKey = apiKey
        , project = projectId
        }
        -- |> Config.withDatabase "your-own-database"
        -- optional
        -- |> Config.withAuthorization "your-own-auth-token"
        -- optional
        |> Firestore.init


apiKey : String
apiKey =
    "AIzaSyCu2fEiCuFiI-bOCFx-J02E6DwziM9HtBg"


authDomain : String
authDomain =
    "miya-library.firebaseapp.com"


projectId : String
projectId =
    "miya-library"


storageBucket : String
storageBucket =
    "miya-library.appspot.com"


messagingSenderId : String
messagingSenderId =
    "268759908765"


appId : String
appId =
    "1:268759908765:web:61d65df9f0948a50832f74"
