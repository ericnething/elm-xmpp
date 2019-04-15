module XMPP.Stanza.Presence
    exposing
    -- Types
    ( Presence
    , Attributes
    , Type

    -- Construct
    , available
    , availableTo
    , unavailable
    , unavailableTo
    , subscribe
    , subscribed
    , unsubscribe
    , unsubscribed
    , probe
    , error

    -- Inspect
    , inspect

    -- XML
    , toXml
    )

import XmlParser exposing (..)
import XMPP.Stanza.Utils exposing (filterAttributes)
import XMPP.JID as JID exposing (JID)


type Presence = Presence Attributes (List Node)

type alias Attributes =
    { to      : Maybe JID
    , from    : Maybe JID
    , type_   : Type
    , id      : Maybe String
    , xmllang : Maybe String
    }


{-
See page 64 of https://datatracker.ietf.org/doc/rfc6121/

 The values of the 'type' attribute can be summarized as follows:

   o  error -- An error has occurred regarding processing of a
      previously sent presence stanza; if the presence stanza is of type
      "error", it MUST include an <error/> child element (refer to
      [XMPP-CORE]).

   o  probe -- A request for an entity's current presence; SHOULD be
      generated only by a server on behalf of a user.

   o  subscribe -- The sender wishes to subscribe to the recipient's
      presence.

   o  subscribed -- The sender has allowed the recipient to receive
      their presence.

   o  unavailable -- The sender is no longer available for
      communication.

   o  unsubscribe -- The sender is unsubscribing from the receiver's
      presence.

   o  unsubscribed -- The subscription request has been denied or a
      previously granted subscription has been canceled.
-}

type Type
    = Available
    | Unavailable
    | Subscribe
    | Subscribed
    | Unsubscribe
    | Unsubscribed
    | Probe
    | Error

-- Constructing

presence : Maybe JID -> Type -> List Node -> Presence
presence to type_ nodes =
    Presence
    { to      = to
    , from    = Nothing
    , type_   = type_
    , id      = Nothing
    , xmllang = Nothing
    }
    nodes

available : List Node -> Presence
available nodes = presence Nothing Available nodes

availableTo : JID -> List Node -> Presence
availableTo to nodes = presence (Just to) Available nodes

unavailable : List Node -> Presence
unavailable nodes = presence Nothing Unavailable nodes

unavailableTo : JID -> List Node -> Presence
unavailableTo to nodes = presence (Just to) Unavailable nodes

subscribe : JID -> List Node -> Presence
subscribe to nodes = presence (Just to) Subscribe nodes

subscribed : JID -> List Node -> Presence
subscribed to nodes = presence (Just to) Subscribed nodes

unsubscribe : JID -> List Node -> Presence
unsubscribe to nodes = presence (Just to) Unsubscribe nodes

unsubscribed : JID -> List Node -> Presence
unsubscribed to nodes = presence (Just to) Unsubscribed nodes

probe : JID -> List Node -> Presence
probe to nodes = presence (Just to) Probe nodes

error : JID -> List Node -> Presence
error to nodes = presence (Just to) Error nodes


-- Inspecting

inspect : Presence -> (Attributes, List Node)
inspect (Presence attrs nodes) = (attrs, nodes)

-- XML

encodeType : Type -> String
encodeType type_ =
    case type_ of
        Available    -> ""
        Unavailable  -> "unavailable"
        Subscribe    -> "subscribe"
        Subscribed   -> "subscribed"
        Unsubscribe  -> "unsubscribe"
        Unsubscribed -> "unsubscribed"
        Probe        -> "probe"
        Error        -> "error"

toXml : Presence -> Node
toXml (Presence { to, from, type_, id, xmllang } nodes) =
    let
        fromJID = Maybe.withDefault "" << Maybe.map JID.toString
        toString = Maybe.withDefault ""
        attrs = filterAttributes
                [ { name = "to",       value = fromJID to }
                , { name = "from",     value = fromJID from }
                , { name = "type",     value = encodeType type_ }
                , { name = "id",       value = toString id }
                , { name = "xml:lang", value = toString xmllang }
                , { name = "xmlns",    value = "jabber:client" }
                ]
    in
        Element "presence" attrs nodes
