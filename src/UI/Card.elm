module UI.Card exposing (eager, lazy)

import Html exposing (Attribute, Html, div, img)
import Html.Attributes exposing (attribute, class, src, title)


type alias Image card =
    { card | title : String, id : String }


lazy : Image card -> Html msg
lazy =
    common (attribute "loading" "lazy")


eager : Image card -> Html msg
eager =
    common (attribute "loading" "eager")


common : Attribute msg -> Image card -> Html msg
common loading provProps =
    div [ class "cardimage" ]
        [ img
            [ class "cardimage"
            , title provProps.title
            , src <| cardSrc provProps.id
            , loading
            ]
            []
        ]


cardSrc : String -> String
cardSrc cardId =
    "/assets/card-" ++ cardId ++ ".webp"
