module API.Cards exposing (fetchCards)

import Cards
import Http
import Json.Decode as Decode exposing (Decoder, andThen, bool, field, index, int, list, map, map2, maybe, string)
import Json.Decode.Pipeline exposing (optional, required)
import List
import Maybe
import String
import Tuple


fetchCards : (Result Http.Error (List PreCard) -> msg) -> Cmd msg
fetchCards msg =
    Http.get
        { url = "https://api.fiveringsdb.com/cards"
        , expect = Http.expectJson msg cardsDecoder
        }


type alias PreCard =
    { allowedClans : List Cards.Clan
    , cardType : String
    , clan : Maybe Cards.Clan
    , cost : Maybe Int
    , deckLimit : Int
    , element : List Cards.Element
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


cardsDecoder : Decoder (List PreCard)
cardsDecoder =
    field "records" (list card)


card : Decoder PreCard
card =
    Decode.succeed PreCard
        |> required "allowed_clans" (mapList toClan)
        |> required "type" string
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



-- |> optional "honor" int
-- |> optional "influence_pool" int
-- |> optional "fate" int


toFormatRequirement =
    Nothing


toRoleTraits : List String -> List Cards.RoleTypes
toRoleTraits =
    let
        nameToType string =
            case string of
                "keeper" ->
                    Just Cards.KeeperRole

                "seeker" ->
                    Just Cards.SeekerRole

                "air" ->
                    Just Cards.AirRole

                "earth" ->
                    Just Cards.EarthRole

                "fire" ->
                    Just Cards.FireRole

                "void" ->
                    Just Cards.VoidRole

                "water" ->
                    Just Cards.WaterRole

                _ ->
                    Nothing
    in
    List.filterMap nameToType



-- magic : List Cards.Clan -> String -> Maybe Cards.Clan -> Maybe Int -> Int -> List Cards.Element -> Maybe Int -> String -> String -> String -> Maybe Int -> Bool -> Bool -> Bool -> Bool -> Bool -> Maybe Int -> Maybe Int -> String -> String -> ( String, Maybe Int ) -> Maybe Int -> Maybe Int -> Maybe String -> String -> Maybe Int -> Maybe Int -> Maybe String -> Maybe String -> List String -> Bool -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Cards.Card
-- magic allowedClans cardType clan cost deckLimit element glory id illustrator imageUrl influenceCost isBanned isBannedInJade isBannedInSkirmish isRestricted isRestrictedInJade military militaryBonus name nameCanonical pack political politicalBonus roleRestriction side strength strengthBonus text textCanonical traits unicity startingHonor influencePool fate =
--     case ( side, cardType ) of
--         ( "role", "role" ) ->
--             Just <|
--                 Cards.RoleCard <|
--                     { title = name
--                     , traits = toRoleTraits traits
--                     , abilities = Maybe.map List.singleton textCanonical |> Maybe.withDefault []
--                     , formatRequirement = toFormatRequirement
--                     , cycle = Tuple.first pack
--                     , cardNumber = Tuple.second pack |> Maybe.withDefault 0
--                     , artist = illustrator
--                     }
--         ( "province", "stronghold" ) ->
--             let
--                 asd a b c d e =
--                     1
--             in
--             Maybe.map5 asd clan strength startingHonor influencePool
--         --     Cards.StrongholdCard
--         -- <|
--         --     { title = name
--         --     , clan = Clan
--         --     , traits = traits
--         --     , strength = Int
--         --     , startingHonor = Int
--         --     , fateValue = Int
--         --     , influenceValue = Int
--         --     , abilities = List String
--         --     , formatRequirement = Maybe Format
--         --     , cycle = String
--         --     , cardNumber = Int
--         --     , artist = String
--         --     }
--         ( "province", "province" ) ->
--             Nothing
--         ( "dynasty", "character" ) ->
--             Nothing
--         ( "dynasty", "event" ) ->
--             Nothing
--         ( "dynasty", "holding" ) ->
--             Nothing
--         ( "conflict", "event" ) ->
--             Nothing
--         ( "conflict", "character" ) ->
--             Nothing
--         ( "conflict", "attachment" ) ->
--             Nothing
--         ( _, _ ) ->
--             Nothing


toInt : Decoder (Maybe Int)
toInt =
    map String.toInt string


mapList : (String -> Maybe to) -> Decoder (List to)
mapList to =
    map (List.filterMap to) (list string)


toClan : String -> Maybe Cards.Clan
toClan str =
    case str of
        "crab" ->
            Just Cards.Crab

        "crane" ->
            Just Cards.Crane

        "dragon" ->
            Just Cards.Dragon

        "lion" ->
            Just Cards.Lion

        "phoenix" ->
            Just Cards.Phoenix

        "scorpion" ->
            Just Cards.Scorpion

        "unicorn" ->
            Just Cards.Unicorn

        _ ->
            Nothing


toElement : String -> Maybe Cards.Element
toElement str =
    case str of
        "air" ->
            Just Cards.Air

        "earth" ->
            Just Cards.Earth

        "fire" ->
            Just Cards.Fire

        "void" ->
            Just Cards.Void

        "water" ->
            Just Cards.Water

        _ ->
            Nothing
