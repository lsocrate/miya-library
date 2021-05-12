module API.Cards exposing (fetchCards)

import Cards
import Http
import Json.Decode as Decode exposing (Decoder, bool, field, index, int, list, map, string)
import Json.Decode.Pipeline exposing (optional, required)
import List
import String


fetchCards : (Result Http.Error (List Cards.Card) -> msg) -> Cmd msg
fetchCards msg =
    Http.get
        { url = "https://api.fiveringsdb.com/cards"
        , expect = Http.expectJson msg cardsDecoder
        }


cardsDecoder : Decoder (List Cards.Card)
cardsDecoder =
    field "records" (list card)


card : Decoder Cards.Card
card =
    Decode.succeed decoderForCardType
        |> required "type" string
        |> required "side" string
        |> Decode.andThen identity


decoderForCardType : String -> String -> Decoder Cards.Card
decoderForCardType cardType_ cardBack =
    case ( cardType_, cardBack ) of
        ( "role", "role" ) ->
            roleDecoder

        ( "province", "stronghold" ) ->
            strongholdDecoder

        ( "province", "province" ) ->
            provinceDecoder

        -- ( "dynasty", "character" ) ->
        --     Just Cards.RoleCard
        -- ( "dynasty", "event" ) ->
        --     Just Cards.RoleCard
        -- ( "dynasty", "holding" ) ->
        --     Just Cards.RoleCard
        -- ( "conflict", "event" ) ->
        --     Just Cards.RoleCard
        -- ( "conflict", "character" ) ->
        --     Just Cards.RoleCard
        -- ( "conflict", "attachment" ) ->
        --     Just Cards.RoleCard
        ( _, _ ) ->
            roleDecoder


roleDecoder : Decoder Cards.Card
roleDecoder =
    Decode.succeed Cards.RoleProperties
        |> title
        |> roleTraits
        |> abilities
        |> formatRequirement
        |> cycle
        |> cardNumber
        |> artist
        |> map Cards.RoleCard


strongholdDecoder : Decoder Cards.Card
strongholdDecoder =
    Decode.succeed Cards.StrongholdProperties
        |> title
        |> clan
        |> traits
        |> bonusStrength
        |> startingHonor
        |> fateValue
        |> influenceValue
        |> abilities
        |> formatRequirement
        |> cycle
        |> cardNumber
        |> artist
        |> map Cards.StrongholdCard


provinceDecoder : Decoder Cards.Card
provinceDecoder =
    Decode.succeed Cards.ProvinceProperties
        |> title
        |> uniqueness
        |> clan
        |> traits
        |> strength
        |> elements
        |> abilities
        |> roleRequirement
        |> formatRequirement
        |> cycle
        |> cardNumber
        |> artist
        |> map Cards.ProvinceCard


roleRequirement : Decoder (Maybe Cards.RoleTypes -> b) -> Decoder b
roleRequirement =
    let
        toRoleRequirement =
            string
                |> Decode.andThen
                    (\str ->
                        case str of
                            "keeper" ->
                                Decode.succeed (Just Cards.KeeperRole)

                            "seeker" ->
                                Decode.succeed (Just Cards.SeekerRole)

                            "air" ->
                                Decode.succeed (Just Cards.AirRole)

                            "earth" ->
                                Decode.succeed (Just Cards.EarthRole)

                            "fire" ->
                                Decode.succeed (Just Cards.FireRole)

                            "void" ->
                                Decode.succeed (Just Cards.VoidRole)

                            "water" ->
                                Decode.succeed (Just Cards.WaterRole)

                            _ ->
                                Decode.fail "Invalid role restriction"
                    )
    in
    optional "role_restriction" toRoleRequirement Nothing


elements : Decoder (List Cards.Element -> b) -> Decoder b
elements =
    let
        element =
            string
                |> Decode.andThen
                    (\str ->
                        case str of
                            "air" ->
                                Decode.succeed Cards.Air

                            "earth" ->
                                Decode.succeed Cards.Earth

                            "fire" ->
                                Decode.succeed Cards.Fire

                            "void" ->
                                Decode.succeed Cards.Void

                            "water" ->
                                Decode.succeed Cards.Water

                            _ ->
                                Decode.fail "Invalid element"
                    )
    in
    required "element" <| list element


influenceValue : Decoder (Int -> b) -> Decoder b
influenceValue =
    required "influence_pool" int


fateValue : Decoder (Int -> b) -> Decoder b
fateValue =
    required "fate" int


startingHonor : Decoder (Int -> b) -> Decoder b
startingHonor =
    required "honor" int


bonusStrength : Decoder (Int -> b) -> Decoder b
bonusStrength =
    required "strength_bonus" modifier


strength : Decoder (Int -> b) -> Decoder b
strength =
    required "strength" modifier


traits : Decoder (List String -> b) -> Decoder b
traits =
    required "traits" <| list string


title : Decoder (String -> b) -> Decoder b
title =
    required "name" string


abilities : Decoder (List String -> b) -> Decoder b
abilities =
    required "text_canonical" <| map (String.split "\n") string


formatRequirement : Decoder (Maybe Cards.Format -> b) -> Decoder b
formatRequirement =
    required "text_canonical" <| map (always Nothing) string


uniqueness : Decoder (Cards.Uniqueness -> b) -> Decoder b
uniqueness =
    let
        toUnique isUnique =
            if isUnique then
                Cards.Unique

            else
                Cards.NonUnique
    in
    required "unicity" <| map toUnique bool


cycle : Decoder (String -> b) -> Decoder b
cycle =
    required "pack_cards" <| index 0 (field "pack" (field "id" string))


cardNumber : Decoder (Int -> b) -> Decoder b
cardNumber =
    required "pack_cards" <| index 0 (field "position" modifier)


artist : Decoder (String -> b) -> Decoder b
artist =
    required "pack_cards" <| index 0 (field "illustrator" string)


modifier : Decoder Int
modifier =
    let
        decodeModifier str =
            case String.toInt str of
                Just n ->
                    Decode.succeed n

                Nothing ->
                    Decode.fail "Invalid modifier"
    in
    string |> Decode.andThen decodeModifier


clan : Decoder (Cards.Clan -> b) -> Decoder b
clan =
    let
        decodeClan str =
            case str of
                "crab" ->
                    Decode.succeed Cards.Crab

                "crane" ->
                    Decode.succeed Cards.Crane

                "dragon" ->
                    Decode.succeed Cards.Dragon

                "lion" ->
                    Decode.succeed Cards.Lion

                "phoenix" ->
                    Decode.succeed Cards.Phoenix

                "scorpion" ->
                    Decode.succeed Cards.Scorpion

                "unicorn" ->
                    Decode.succeed Cards.Unicorn

                _ ->
                    Decode.fail "Invalid clan"
    in
    required "clan" <| (string |> Decode.andThen decodeClan)


roleTraits : Decoder (List Cards.RoleTypes -> b) -> Decoder b
roleTraits =
    let
        toRoleTrait string =
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
    required "traits" <| map (List.filterMap toRoleTrait) (list string)
