module Xmpp.Stanza
    exposing (..)
    -- ( encode
    -- , json
    -- )

import Xmpp.Xml exposing (..)
import Xmpp.Stanza.Message as Message exposing (Message)
import Xmpp.Stanza.Presence as Presence exposing (Presence)
import Xmpp.Stanza.Iq as Iq exposing (Iq)
import Xmpp.JID as JID exposing (JID)
import Xmpp.Xml.Decode as XD


encode : Xml -> String
encode = format


{-| JSON containers as defined in [XEP-0335][xep-0335].

[xep-0335]: https://xmpp.org/extensions/xep-0335.html

 -}
json : String -> Node
json s =
    Element "json" [ Attribute "xmlns" "urn:xmpp:json:0" ] [ Text s ]


type Stanza
    = MessageStanza Message
    | PresenceStanza Presence
    | IqStanza Iq

fromXml : Xml -> Result String Stanza
fromXml doc =
    case doc.root of
        Element "message" _ _ ->
            Result.map MessageStanza (Message.fromXml doc)

        Element "presence" _ _ ->
            Result.map PresenceStanza (Presence.fromXml doc)

        Element "iq" _ _ ->
            Result.map IqStanza (Iq.fromXml doc)

        _ ->
            Err "not a valid stanza"
        
