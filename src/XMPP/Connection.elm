module XMPP.Connection exposing (..)

import XmlParser
import XMPP.JID as JID exposing (JID)

{-
outgoing      <open>
incoming      <open>
incoming      <features>
outgoing      <auth>
incoming      <challenge>
outgoing      <response>
incoming      <success>
outgoing      <open>
incoming      <open>
incoming      <features>
outgoing      <iq>
incoming      <iq>
outgoing      <enable sm>
incoming      <enabled sm>
outgoing      <r sm>
outgoing      <iq xmpp-session>
incoming      <a sm>
 -}

-- type alias Client =
--     { wsClient : WebSocket
--     , jid : JID
--     }


type alias Config =
    { jid : JID
    , credentials : Credentials
    , url : String
    , useStreamManagement : Bool
    }

type Credentials
    = Password String
    | JWT String
