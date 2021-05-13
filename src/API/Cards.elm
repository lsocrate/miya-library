module API.Cards exposing (fetchCards)

import Card
import Clan
import Debug exposing (toString)
import Format
import Http
import Json.Decode as Decode exposing (Decoder, bool, field, index, int, list, map, maybe, string)
import Json.Decode.Pipeline exposing (optional, required)
import List
import String


fetchCards : (Result Http.Error (List Card.Card) -> msg) -> Cmd msg
fetchCards msg =
    Http.get
        { url = "https://api.fiveringsdb.com/cards"
        , expect = Http.expectJson msg cardsDecoder
        }


cardsDecoder : Decoder (List Card.Card)
cardsDecoder =
    field "records" (list card) |> map (List.filterMap identity)


card : Decoder (Maybe Card.Card)
card =
    let
        decodeCard decoder =
            case decoder of
                Just cardDecoder ->
                    map Just cardDecoder

                Nothing ->
                    Decode.succeed Nothing
    in
    Decode.succeed decoderForCardType
        |> required "type" string
        |> required "side" string
        |> Decode.andThen decodeCard


decoderForCardType : String -> String -> Maybe (Decoder Card.Card)
decoderForCardType cardBack cardType =
    case ( cardType, cardBack ) of
        ( "province", "stronghold" ) ->
            Just strongholdDecoder

        ( "role", "role" ) ->
            Just roleDecoder

        ( "province", "province" ) ->
            Just provinceDecoder

        ( "dynasty", "holding" ) ->
            Just holdingDecoder

        ( "conflict", "attachment" ) ->
            Just attachmentDecoder

        ( "dynasty", "event" ) ->
            Just dynastyEventDecoder

        ( "conflict", "event" ) ->
            Just conflictEventDecoder

        ( "dynasty", "character" ) ->
            Just dynastyCharacterDecoder

        ( "conflict", "character" ) ->
            Just conflictCharacterDecoder

        ( _, _ ) ->
            Nothing



-- CARD TYPE DECODERS


strongholdDecoder : Decoder Card.Card
strongholdDecoder =
    Decode.succeed Card.StrongholdProps
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
        |> map (Card.StrongholdType << Card.Stronghold)


roleDecoder : Decoder Card.Card
roleDecoder =
    Decode.succeed Card.RoleProps
        |> title
        |> roleTraits
        |> abilities
        |> formatRequirement
        |> cycle
        |> cardNumber
        |> artist
        |> map (Card.RoleType << Card.Role)


provinceDecoder : Decoder Card.Card
provinceDecoder =
    Decode.succeed Card.ProvinceProps
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
        |> map (Card.ProvinceType << Card.Province)


holdingDecoder : Decoder Card.Card
holdingDecoder =
    Decode.succeed Card.HoldingProps
        |> title
        |> uniqueness
        |> clan
        |> traits
        |> bonusStrength
        |> abilities
        |> roleRequirement
        |> formatRequirement
        |> cycle
        |> cardNumber
        |> artist
        |> map (Card.HoldingType << Card.Holding)


attachmentDecoder : Decoder Card.Card
attachmentDecoder =
    Decode.succeed Card.AttachmentProps
        |> title
        |> uniqueness
        |> clan
        |> traits
        |> cost
        |> skillBonus Military
        |> skillBonus Political
        |> abilities
        |> influenceCost
        |> roleRequirement
        |> formatRequirement
        |> cycle
        |> cardNumber
        |> artist
        |> map (Card.AttachmentType << Card.Attachment)


dynastyEventDecoder : Decoder Card.Card
dynastyEventDecoder =
    Decode.succeed Card.DynastyEventProps
        |> title
        |> clan
        |> traits
        |> cost
        |> abilities
        |> roleRequirement
        |> formatRequirement
        |> cycle
        |> cardNumber
        |> artist
        |> map (Card.EventType << Card.DynastyEvent)


conflictEventDecoder : Decoder Card.Card
conflictEventDecoder =
    Decode.succeed Card.ConflictEventProps
        |> title
        |> clan
        |> traits
        |> cost
        |> abilities
        |> influenceCost
        |> roleRequirement
        |> formatRequirement
        |> cycle
        |> cardNumber
        |> artist
        |> map (Card.EventType << Card.ConflictEvent)


dynastyCharacterDecoder : Decoder Card.Card
dynastyCharacterDecoder =
    Decode.succeed Card.DynastyCharacterProps
        |> title
        |> uniqueness
        |> clan
        |> traits
        |> cost
        |> skill Military
        |> skill Political
        |> glory
        |> abilities
        |> roleRequirement
        |> formatRequirement
        |> cycle
        |> cardNumber
        |> artist
        |> map (Card.CharacterType << Card.DynastyCharacter)


conflictCharacterDecoder : Decoder Card.Card
conflictCharacterDecoder =
    Decode.succeed Card.ConflictCharacterProps
        |> title
        |> uniqueness
        |> clan
        |> traits
        |> cost
        |> skill Military
        |> skill Political
        |> glory
        |> abilities
        |> influenceCost
        |> roleRequirement
        |> formatRequirement
        |> cycle
        |> cardNumber
        |> artist
        |> map (Card.CharacterType << Card.ConflictCharacter)



-- PROPERTY DECODERS


glory : Decoder (Int -> b) -> Decoder b
glory =
    required "glory" int


type SkillType
    = Military
    | Political


skill : SkillType -> Decoder (Card.NumericalValue -> b) -> Decoder b
skill skillType =
    let
        fieldName =
            case skillType of
                Military ->
                    "military"

                Political ->
                    "political"
    in
    required fieldName (string |> Decode.andThen toNumericalValue)


toNumericalValue : String -> Decoder Card.NumericalValue
toNumericalValue string =
    case string of
        "x" ->
            Decode.succeed Card.XValue

        "X" ->
            Decode.succeed Card.XValue

        _ ->
            case String.toInt string of
                Just n ->
                    Decode.succeed (Card.FixedValue n)

                Nothing ->
                    Decode.fail "Invalid numerical value"


skillBonus : SkillType -> Decoder (Card.NumericalModifier -> b) -> Decoder b
skillBonus skillType =
    let
        fieldName =
            case skillType of
                Military ->
                    "military_bonus"

                Political ->
                    "political_bonus"

        toSkillModifier str =
            if str == "x" then
                Decode.succeed Card.XModifier

            else
                case String.toInt str of
                    Just n ->
                        Decode.succeed (Card.FixedModifier n)

                    Nothing ->
                        Decode.fail "Invalid cost"
    in
    required fieldName (string |> Decode.andThen toSkillModifier)


cost : Decoder (Card.NumericalValue -> b) -> Decoder b
cost =
    let
        toCost str =
            if str == "x" then
                Decode.succeed Card.XValue

            else
                case String.toInt str of
                    Just n ->
                        Decode.succeed (Card.FixedValue n)

                    Nothing ->
                        Decode.fail "Invalid cost"
    in
    optional "cost" (string |> Decode.andThen toCost) Card.DashValue


influenceCost : Decoder (Maybe Int -> b) -> Decoder b
influenceCost =
    optional "influence_cost" (maybe int) Nothing


roleRequirement : Decoder (Maybe Card.RoleTypes -> b) -> Decoder b
roleRequirement =
    let
        toRoleRequirement =
            string
                |> Decode.andThen
                    (\str ->
                        case str of
                            "keeper" ->
                                Decode.succeed (Just Card.KeeperRole)

                            "seeker" ->
                                Decode.succeed (Just Card.SeekerRole)

                            "air" ->
                                Decode.succeed (Just Card.AirRole)

                            "earth" ->
                                Decode.succeed (Just Card.EarthRole)

                            "fire" ->
                                Decode.succeed (Just Card.FireRole)

                            "void" ->
                                Decode.succeed (Just Card.VoidRole)

                            "water" ->
                                Decode.succeed (Just Card.WaterRole)

                            _ ->
                                Decode.fail "Invalid role restriction"
                    )
    in
    optional "role_restriction" toRoleRequirement Nothing


elements : Decoder (List Card.Element -> b) -> Decoder b
elements =
    let
        element =
            string
                |> Decode.andThen
                    (\str ->
                        case str of
                            "air" ->
                                Decode.succeed Card.Air

                            "earth" ->
                                Decode.succeed Card.Earth

                            "fire" ->
                                Decode.succeed Card.Fire

                            "void" ->
                                Decode.succeed Card.Void

                            "water" ->
                                Decode.succeed Card.Water

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


strength : Decoder (Card.NumericalValue -> b) -> Decoder b
strength =
    required "strength" (string |> Decode.andThen toNumericalValue)


traits : Decoder (List String -> b) -> Decoder b
traits =
    required "traits" <| list string


title : Decoder (String -> b) -> Decoder b
title =
    required "name" string


abilities : Decoder (List String -> b) -> Decoder b
abilities =
    required "text_canonical" <| map (String.split "\n") string


formatRequirement : Decoder (Maybe Format.Format -> b) -> Decoder b
formatRequirement =
    required "text_canonical" <| map (always Nothing) string


uniqueness : Decoder (Card.Uniqueness -> b) -> Decoder b
uniqueness =
    let
        toUnique isUnique =
            if isUnique then
                Card.Unique

            else
                Card.NonUnique
    in
    required "unicity" <| map toUnique bool


cycle : Decoder (String -> b) -> Decoder b
cycle =
    required "pack_cards" <| index 0 (field "pack" (field "id" string))


cardNumber : Decoder (String -> b) -> Decoder b
cardNumber =
    required "pack_cards" <| index 0 (field "position" string)


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


clan : Decoder (Clan.Clan -> b) -> Decoder b
clan =
    let
        decodeClan str =
            case str of
                "crab" ->
                    Decode.succeed Clan.Crab

                "crane" ->
                    Decode.succeed Clan.Crane

                "dragon" ->
                    Decode.succeed Clan.Dragon

                "lion" ->
                    Decode.succeed Clan.Lion

                "phoenix" ->
                    Decode.succeed Clan.Phoenix

                "scorpion" ->
                    Decode.succeed Clan.Scorpion

                "unicorn" ->
                    Decode.succeed Clan.Unicorn

                "neutral" ->
                    Decode.succeed Clan.Neutral

                _ ->
                    Decode.fail "Invalid clan"
    in
    required "clan" <| (string |> Decode.andThen decodeClan)


roleTraits : Decoder (List Card.RoleTypes -> b) -> Decoder b
roleTraits =
    let
        toRoleTrait string =
            case string of
                "keeper" ->
                    Just Card.KeeperRole

                "seeker" ->
                    Just Card.SeekerRole

                "air" ->
                    Just Card.AirRole

                "earth" ->
                    Just Card.EarthRole

                "fire" ->
                    Just Card.FireRole

                "void" ->
                    Just Card.VoidRole

                "water" ->
                    Just Card.WaterRole

                _ ->
                    Nothing
    in
    required "traits" <| map (List.filterMap toRoleTrait) (list string)
