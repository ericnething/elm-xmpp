module XMPP.Connection exposing (..)

import XmlParser
import XMPP.JID as JID exposing (JID)


type alias Client =
    { wsClient : WebSocket
    , jid : JID
    }


type alias Config =
    { jid : JID
    , credentials : Credentials
    , url : String
    , useStreamManagement : Bool
    }

type Credentials
    = Password String
    | JWT String
