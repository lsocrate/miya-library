module API.Cards exposing (Card, fetchCards)

import Http
import Json.Decode as Decode exposing (Decoder, bool, field, index, int, list, map, map2, maybe, string)
import Json.Decode.Pipeline exposing (optional, required)
import Rules.Cards
import Rules.Clans
import Rules.Elements
import String
import Tuple


fetchCards : (Result Http.Error (List Card) -> msg) -> Cmd msg
fetchCards msg =
    Http.get
        { url = "https://api.fiveringsdb.com/cards"
        , expect = Http.expectJson msg cardsDecoder
        }


type alias Card =
    { allowedClans : List Rules.Clans.Clan
    , cardType : Maybe Rules.Cards.CardType
    , clan : Maybe Rules.Clans.Clan
    , cost : Maybe Int
    , deckLimit : Int
    , element : List Rules.Elements.Element
    , glory : Maybe Int
    , id : String
    , illustrator : String
    , imageUrl : String
    , influenceCost : Maybe Int
    , isBanned : Bool
    , isBannedInJade : Bool
    , isBannedInSkirmish : Bool
    , isRestricted : Bool
    , isRestrictedInJade : Bool
    , military : Maybe Int
    , militaryBonus : Maybe Int
    , name : String
    , nameCanonical : String
    , pack : ( String, Maybe Int )
    , political : Maybe Int
    , politicalBonus : Maybe Int
    , roleRestriction : Maybe String
    , side : String
    , strength : Maybe Int
    , strengthBonus : Maybe Int
    , text : Maybe String
    , textCanonical : Maybe String
    , traits : List String
    , unicity : Bool
    }


cardsDecoder : Decoder (List Card)
cardsDecoder =
    field "records" (list card)


card : Decoder Card
card =
    Decode.succeed Card
        |> required "allowed_clans" (mapList toClan)
        |> required "type" (map toCardType string)
        |> required "clan" (map toClan string)
        |> optional "cost" toInt Nothing
        |> required "deck_limit" int
        |> required "element" (mapList toElement)
        |> optional "glory" (maybe int) Nothing
        |> required "id" string
        |> required "pack_cards" (index 0 (field "illustrator" string))
        |> required "pack_cards" (index 0 (field "image_url" string))
        |> optional "influence_cost" (maybe int) Nothing
        |> required "is_banned" bool
        |> required "is_banned_in_jade" bool
        |> required "is_banned_in_skirmish" bool
        |> required "is_restricted" bool
        |> required "is_restricted_in_jade" bool
        |> optional "military" toInt Nothing
        |> optional "military_bonus" toInt Nothing
        |> required "name" string
        |> required "name_canonical" string
        |> required "pack_cards" (index 0 (map2 Tuple.pair (field "pack" (field "id" string)) (field "position" toInt)))
        |> optional "political" toInt Nothing
        |> optional "political_bonus" toInt Nothing
        |> optional "role_restrictions" (maybe string) Nothing
        |> required "side" string
        |> optional "strength" toInt Nothing
        |> optional "strength_bonus" toInt Nothing
        |> optional "text" (maybe string) Nothing
        |> optional "text_canonical" (maybe string) Nothing
        |> required "traits" (list string)
        |> required "unicity" bool


toInt : Decoder (Maybe Int)
toInt =
    map String.toInt string


mapList : (String -> Maybe to) -> Decoder (List to)
mapList to =
    map (List.filterMap to) (list string)


toClan : String -> Maybe Rules.Clans.Clan
toClan str =
    case str of
        "crab" ->
            Just Rules.Clans.Crab

        "crane" ->
            Just Rules.Clans.Crane

        "dragon" ->
            Just Rules.Clans.Dragon

        "lion" ->
            Just Rules.Clans.Lion

        "phoenix" ->
            Just Rules.Clans.Phoenix

        "scorpion" ->
            Just Rules.Clans.Scorpion

        "unicorn" ->
            Just Rules.Clans.Unicorn

        _ ->
            Nothing


toElement : String -> Maybe Rules.Elements.Element
toElement str =
    case str of
        "air" ->
            Just Rules.Elements.Air

        "earth" ->
            Just Rules.Elements.Earth

        "fire" ->
            Just Rules.Elements.Fire

        "void" ->
            Just Rules.Elements.Void

        "water" ->
            Just Rules.Elements.Water

        _ ->
            Nothing


toCardType : String -> Maybe Rules.Cards.CardType
toCardType str =
    case str of
        "province" ->
            Just Rules.Cards.Province

        "attachment" ->
            Just Rules.Cards.Attachment

        "character" ->
            Just Rules.Cards.Character

        "holding" ->
            Just Rules.Cards.Holding

        "event" ->
            Just Rules.Cards.Event

        "role" ->
            Just Rules.Cards.Role

        "stronghold" ->
            Just Rules.Cards.Stronghold

        _ ->
            Nothing
