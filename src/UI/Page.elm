module UI.Page exposing (view)

import Gen.Route as Route
import Html exposing (..)
import Html.Attributes exposing (..)
import List
import View exposing (View)


view : Route.Route -> List (Html msg) -> View msg
view currentRoute mainViews =
    let
        items =
            [ ( "Deckbuilder", Route.Deckbuilder ) ]

        navItem ( name, target ) =
            li
                [ classList
                    [ ( "nav-item", True )
                    , ( "nav-item--active", currentRoute == target )
                    ]
                ]
                [ a [ href <| Route.toHref target ] [ text name ] ]
    in
    { title = "Miya Library"
    , body =
        [ header [ class "page-header" ]
            [ div [ class "header-logo" ] [ text "Miya Library" ]
            , nav [ class "nav" ]
                [ ul [ class "nav-list" ] <| List.map navItem items ]
            ]
        , main_ [ class "page-main" ] mainViews
        , footer [ class "page-footer" ] [ text "footer" ]
        ]
    }
