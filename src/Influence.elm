module Influence exposing (InfluenceCost(..), toInt, toString)


type InfluenceCost
    = None
    | InfluenceCost1
    | InfluenceCost2
    | InfluenceCost3
    | InfluenceCost4


toString : InfluenceCost -> String
toString influenceCost =
    case influenceCost of
        None ->
            "influence0"

        InfluenceCost1 ->
            "influence1"

        InfluenceCost2 ->
            "influence2"

        InfluenceCost3 ->
            "influence3"

        InfluenceCost4 ->
            "influence4"


toInt : InfluenceCost -> Int
toInt influenceCost =
    case influenceCost of
        None ->
            0

        InfluenceCost1 ->
            1

        InfluenceCost2 ->
            2

        InfluenceCost3 ->
            3

        InfluenceCost4 ->
            4
