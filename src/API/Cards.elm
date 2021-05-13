module API.Cards exposing (fetchCards)

import Cards
import Http
import Json.Decode as Decode exposing (Decoder, bool, field, index, int, list, map, maybe, string)
import Json.Decode.Pipeline exposing (optional, required)
import List
import String


fetchCards : (Result Http.Error (List Cards.ACard) -> msg) -> Cmd msg
fetchCards msg =
    Http.get
        { url = "https://api.fiveringsdb.com/cards"
        , expect = Http.expectJson msg cardsDecoder
        }


cardsDecoder : Decoder (List Cards.ACard)
cardsDecoder =
    field "records" (list card) |> map (List.filterMap identity)


card : Decoder (Maybe Cards.ACard)
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


decoderForCardType : String -> String -> Maybe (Decoder Cards.ACard)
decoderForCardType cardType_ cardBack =
    case ( cardType_, cardBack ) of
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
            Just conflictharacterDecoder

        ( _, _ ) ->
            Nothing



-- CARD TYPE DECODERS


strongholdDecoder : Decoder Cards.ACard
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
        |> map (Cards.AStronghold << Cards.BStronghold)


roleDecoder : Decoder Cards.ACard
roleDecoder =
    Decode.succeed Cards.RoleProperties
        |> title
        |> roleTraits
        |> abilities
        |> formatRequirement
        |> cycle
        |> cardNumber
        |> artist
        |> map (Cards.ARole << Cards.BRole)


provinceDecoder : Decoder Cards.ACard
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
        |> map (Cards.AProvince << Cards.BProvince)


holdingDecoder : Decoder Cards.ACard
holdingDecoder =
    Decode.succeed Cards.DynastyHoldingPropperties
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
        |> map (Cards.AHolding << Cards.BHolding)


attachmentDecoder : Decoder Cards.ACard
attachmentDecoder =
    Decode.succeed Cards.ConflictAttachmentProperties
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
        |> map (Cards.AAttachment << Cards.BAttachment)


dynastyEventDecoder : Decoder Cards.ACard
dynastyEventDecoder =
    Decode.succeed Cards.DynastyEventProperties
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
        |> map (Cards.AEvent << Cards.BDynEvent)


conflictEventDecoder : Decoder Cards.ACard
conflictEventDecoder =
    Decode.succeed Cards.ConflictEventProperties
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
        |> map (Cards.AEvent << Cards.BConfEvent)


dynastyCharacterDecoder : Decoder Cards.ACard
dynastyCharacterDecoder =
    Decode.succeed Cards.DynastyCharacterProperties
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
        |> map (Cards.AChar << Cards.BDynChar)


conflictharacterDecoder : Decoder Cards.ACard
conflictharacterDecoder =
    Decode.succeed Cards.ConflictCharacterPropperties
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
        |> map (Cards.AChar << Cards.BConfChar)



-- PROPERTY DECODERS


glory : Decoder (Int -> b) -> Decoder b
glory =
    required "glory" int


type SkillType
    = Military
    | Political


skill : SkillType -> Decoder (Cards.NumericalValue -> b) -> Decoder b
skill skillType =
    let
        fieldName =
            case skillType of
                Military ->
                    "military"

                Political ->
                    "political"

        toSkill str =
            if str == "x" then
                Decode.succeed Cards.XValue

            else
                case String.toInt str of
                    Just n ->
                        Decode.succeed (Cards.FixedValue n)

                    Nothing ->
                        Decode.fail "Invalid skill value"
    in
    required fieldName (string |> Decode.andThen toSkill)


skillBonus : SkillType -> Decoder (Cards.NumericalModifier -> b) -> Decoder b
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
                Decode.succeed Cards.XModifier

            else
                case String.toInt str of
                    Just n ->
                        Decode.succeed (Cards.FixedModifier n)

                    Nothing ->
                        Decode.fail "Invalid cost"
    in
    required fieldName (string |> Decode.andThen toSkillModifier)


cost : Decoder (Cards.NumericalValue -> b) -> Decoder b
cost =
    let
        toCost str =
            if str == "x" then
                Decode.succeed Cards.XValue

            else
                case String.toInt str of
                    Just n ->
                        Decode.succeed (Cards.FixedValue n)

                    Nothing ->
                        Decode.fail "Invalid cost"
    in
    optional "cost" (string |> Decode.andThen toCost) Cards.DashValue


influenceCost : Decoder (Maybe Int -> b) -> Decoder b
influenceCost =
    optional "influence_cost" (maybe int) Nothing


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
