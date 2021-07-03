module DeckMeta exposing (..)

import Clan exposing (Clan)
import Format exposing (Format)


type alias DeckMeta =
    { id : String
    , authorId : String
    , format : Format
    , clan : Clan
    , name : Maybe String
    , description : Maybe String
    }
