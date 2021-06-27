module Uniqueness exposing (..)


type Uniqueness
    = Unique
    | NonUnique


toString : Uniqueness -> String
toString uniqueness =
    case uniqueness of
        Unique ->
            "unique"

        NonUnique ->
            "nonunique"
