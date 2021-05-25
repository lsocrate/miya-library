module Numerical exposing (Numerical(..), toString)


type Numerical
    = Dash
    | FixedValue Int
    | VariableValue
    | FixedModifier Int
    | VariableModifier


toString : Numerical -> String
toString num =
    case num of
        Dash ->
            "–"

        FixedValue n ->
            String.fromInt n

        VariableValue ->
            "X"

        FixedModifier n ->
            if n < 0 then
                String.fromInt n

            else
                "+" ++ String.fromInt n

        VariableModifier ->
            "+X"
