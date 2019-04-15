module XMPP.Stanza
    exposing (..)
    -- ( encode
    -- , json
    -- )

import XmlParser exposing (..)
import XMPP.Stanza.Message as Message exposing (Message)
import XMPP.Stanza.Presence as Presence exposing (Presence)
import XMPP.Stanza.Iq as Iq exposing (Iq)
import XMPP.JID as JID


encode : Node -> String
encode stanza = format <| Xml [] Nothing <| stanza



{-| JSON containers as defined in [XEP-0335][xep-335].

[xep-335]: https://xmpp.org/extensions/xep-0335.html
 -}
json : String -> Node
json s =
    Element "json" [ Attribute "xmlns" "urn:xmpp:json:0" ] [ Text s ]


-- Tests

type Stanza
    = MessageStanza Message
    | PresenceStanza Presence
    | IqStanza Iq

extractMessage : Stanza -> Maybe Message
extractMessage stanza =
    case stanza of
        MessageStanza message -> Just message
        _ -> Nothing

welkin = JID.new "welkin" "localhost" ""

testcase1 =
    Message.toXml <|
        Message.chat welkin
            [ Message.body "Hello"
            , Element "diceroll"
                [ Attribute "xmlns" "roll2d6.org/diceroll" ]
                [ json """{ "dicetype": "fate" }""" ]
            ]

decodeStanza : Node -> Result String Stanza
decodeStanza node =
    case node of
        Element "message" _ _ ->
            Result.map MessageStanza (Message.fromXml node)

        Element "presence" _ _ ->
            Result.map PresenceStanza (Presence.fromXml node)

        Element "iq" _ _ ->
            Result.map IqStanza (Iq.fromXml node)

        _ ->
            Err "not a valid stanza"
        

-- path : String -> Node -> Maybe Node
-- path name node =
--     case node of
--         Element tag attrs [] ->
--             if tag == name then
--                 Just node
--             else
--                 Nothing

--         Element tag attrs children ->
--             if tag == name then
--                 Just node
--             else
--                 path name children

--         Text s ->
--             Nothing
            
