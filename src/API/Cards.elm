module API.Cards exposing (fetchCards)

import Card
import Clan
import Dict
import Format
import Http
import Json.Decode as Decode exposing (Decoder, bool, int, list, map, maybe, string)
import Json.Decode.Pipeline exposing (optional, required)
import List
import Numerical
import String


fetchCards : (Result Http.Error (Dict.Dict String Card.Card) -> msg) -> Cmd msg
fetchCards msg =
    Http.get
        { url = "/assets/cards.json"
        , expect = Http.expectJson msg cardsDecoder
        }


cardsDecoder : Decoder (Dict.Dict String Card.Card)
cardsDecoder =
    list card
        |> map (List.filterMap <| Maybe.map toCardIdTuple)
        |> map Dict.fromList


toCardIdTuple : Card.Card -> ( String, Card.Card )
toCardIdTuple c =
    ( Card.id c, c )


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
        |> id
        |> title
        |> clan
        |> traits
        |> bonusStrength
        |> startingHonor
        |> fateValue
        |> influenceValue
        |> abilities
        |> formatRequirement
        |> image
        |> cycle
        |> cardNumber
        |> artist
        |> map (Card.StrongholdType << Card.Stronghold)


roleDecoder : Decoder Card.Card
roleDecoder =
    Decode.succeed Card.RoleProps
        |> id
        |> title
        |> roleTraits
        |> abilities
        |> formatRequirement
        |> image
        |> cycle
        |> cardNumber
        |> artist
        |> map (Card.RoleType << Card.Role)


provinceDecoder : Decoder Card.Card
provinceDecoder =
    Decode.succeed Card.ProvinceProps
        |> id
        |> title
        |> uniqueness
        |> clan
        |> traits
        |> strength
        |> elements
        |> abilities
        |> roleRequirement
        |> formatRequirement
        |> image
        |> cycle
        |> cardNumber
        |> artist
        |> map (Card.ProvinceType << Card.Province)


holdingDecoder : Decoder Card.Card
holdingDecoder =
    Decode.succeed Card.HoldingProps
        |> id
        |> title
        |> uniqueness
        |> clan
        |> traits
        |> bonusStrength
        |> abilities
        |> roleRequirement
        |> formatRequirement
        |> image
        |> cycle
        |> cardNumber
        |> artist
        |> map (Card.HoldingType << Card.Holding)


attachmentDecoder : Decoder Card.Card
attachmentDecoder =
    Decode.succeed Card.AttachmentProps
        |> id
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
        |> image
        |> cycle
        |> cardNumber
        |> artist
        |> map (Card.AttachmentType << Card.Attachment)


dynastyEventDecoder : Decoder Card.Card
dynastyEventDecoder =
    Decode.succeed Card.DynastyEventProps
        |> id
        |> title
        |> clan
        |> traits
        |> cost
        |> abilities
        |> roleRequirement
        |> formatRequirement
        |> image
        |> cycle
        |> cardNumber
        |> artist
        |> map (Card.EventType << Card.DynastyEvent)


conflictEventDecoder : Decoder Card.Card
conflictEventDecoder =
    Decode.succeed Card.ConflictEventProps
        |> id
        |> title
        |> clan
        |> traits
        |> cost
        |> abilities
        |> influenceCost
        |> roleRequirement
        |> formatRequirement
        |> image
        |> cycle
        |> cardNumber
        |> artist
        |> map (Card.EventType << Card.ConflictEvent)


dynastyCharacterDecoder : Decoder Card.Card
dynastyCharacterDecoder =
    Decode.succeed Card.DynastyCharacterProps
        |> id
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
        |> image
        |> cycle
        |> cardNumber
        |> artist
        |> map (Card.CharacterType << Card.DynastyCharacter)


conflictCharacterDecoder : Decoder Card.Card
conflictCharacterDecoder =
    Decode.succeed Card.ConflictCharacterProps
        |> id
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
        |> image
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


skill : SkillType -> Decoder (Numerical.Numerical -> b) -> Decoder b
skill skillType =
    let
        fieldName =
            case skillType of
                Military ->
                    "military"

                Political ->
                    "political"
    in
    optional fieldName (string |> Decode.andThen toNumericalValue) Numerical.Dash


toNumericalValue : String -> Decoder Numerical.Numerical
toNumericalValue string =
    case string of
        "+X" ->
            Decode.succeed Numerical.VariableModifier

        "X" ->
            Decode.succeed Numerical.VariableValue

        "-" ->
            Decode.succeed Numerical.Dash

        _ ->
            case ( String.left 1 string, String.toInt string ) of
                ( "+", Just n ) ->
                    Decode.succeed (Numerical.FixedModifier n)

                ( "-", Just n ) ->
                    Decode.succeed (Numerical.FixedModifier n)

                ( _, Just n ) ->
                    Decode.succeed (Numerical.FixedValue n)

                ( _, Nothing ) ->
                    Decode.fail "Invalid numerical value"


skillBonus : SkillType -> Decoder (Numerical.Numerical -> b) -> Decoder b
skillBonus skillType =
    let
        fieldName =
            case skillType of
                Military ->
                    "military_bonus"

                Political ->
                    "political_bonus"
    in
    required fieldName (string |> Decode.andThen toNumericalValue)


cost : Decoder (Numerical.Numerical -> b) -> Decoder b
cost =
    optional "cost" (string |> Decode.andThen toNumericalValue) Numerical.Dash


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


elements : Decoder (Card.ProvinceElement -> b) -> Decoder b
elements =
    let
        toElementTuple elList =
            case elList of
                [ elA ] ->
                    Decode.succeed (Card.Single elA)

                [ elA, elB ] ->
                    Decode.succeed (Card.Double elA elB)

                [ _, _, _, _, _ ] ->
                    Decode.succeed Card.Tomoe

                _ ->
                    Decode.fail "Invalid province element combination"

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
    required "element" <| Decode.andThen toElementTuple <| list element


influenceValue : Decoder (Int -> b) -> Decoder b
influenceValue =
    required "influence_pool" int


fateValue : Decoder (Int -> b) -> Decoder b
fateValue =
    required "fate" int


startingHonor : Decoder (Int -> b) -> Decoder b
startingHonor =
    required "honor" int


bonusStrength : Decoder (Numerical.Numerical -> b) -> Decoder b
bonusStrength =
    required "strength_bonus" (string |> Decode.andThen toNumericalValue)


strength : Decoder (Numerical.Numerical -> b) -> Decoder b
strength =
    required "strength" (string |> Decode.andThen toNumericalValue)


traits : Decoder (List String -> b) -> Decoder b
traits =
    required "traits" <| list string


title : Decoder (String -> b) -> Decoder b
title =
    required "name" string


id : Decoder (String -> b) -> Decoder b
id =
    required "id" string


abilities : Decoder (List String -> b) -> Decoder b
abilities =
    optional "text_canonical" (map (String.split "\n") string) []


formatRequirement : Decoder (Maybe Format.Format -> b) -> Decoder b
formatRequirement =
    optional "text_canonical" (map (always Nothing) string) Nothing


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
    required "cycle" string


cardNumber : Decoder (String -> b) -> Decoder b
cardNumber =
    required "card_number" string


artist : Decoder (String -> b) -> Decoder b
artist =
    required "artist" string


image : Decoder (String -> b) -> Decoder b
image =
    required "id" <| map (\cardId -> "/assets/card-" ++ cardId ++ ".webp") string


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

                "shadowlands" ->
                    Decode.succeed Clan.Shadowlands

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
