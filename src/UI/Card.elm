module UI.Card exposing (img)

import Html
import Html.Attributes exposing (attribute, class, src, title)


img : { a | title : String, id : String } -> Html.Html msg
img provProps =
    Html.img
        [ class "cardimage"
        , title provProps.title
        , src <| cardSrc provProps.id
        , attribute "loading" "lazy"
        ]
        []


cardSrc : String -> String
cardSrc cardId =
    "/assets/card-" ++ cardId ++ ".webp"
