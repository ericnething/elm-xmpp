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
    , fromXml
    )

{-| Presence

# Types

@doc Presence, Attributes, Type

# Constructing Presence Stanzas

@doc available, availableTo, unavailable, unavailableTo, subscribe, subscribed, unsubscribe, unsubscribed, probe, error

# Inspecting a Presence Stanza

@doc inspect

# Encoding to XML

@doc toXml

# Decoding from XML

@doc fromXml

 -}

import XmlParser exposing (..)
import XMPP.Stanza.Utils as Utils
import XMPP.JID as JID exposing (JID)

------------------------------------------------------------
-- Types
------------------------------------------------------------

type Presence = Presence Attributes (List Node)

type alias Attributes =
    { to      : Maybe JID
    , from    : Maybe JID
    , type_   : Type
    , id      : Maybe String
    , xmllang : Maybe String
    }

def : Attributes
def =
    { to      = Nothing
    , from    = Nothing
    , type_   = Available
    , id      = Nothing
    , xmllang = Nothing
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

------------------------------------------------------------
-- Constructing Presence Stanzas
------------------------------------------------------------

{-| Generic presence constuctor meant for internal use only.

 -}
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


{-| Construct an `available` presence stanza.

Use this to construct a presence that signals you are available to
receive stanzas.

 -}
available : List Node -> Presence
available nodes = presence Nothing Available nodes


{-| Construct an `available` presence stanza.

Use this when you want to send an `available` presence to a specific
entity instead of broadcasting through the server.

 -}
availableTo : JID -> List Node -> Presence
availableTo to nodes = presence (Just to) Available nodes


{-| Construct an `unavailable` presence stanza.

Use this to construct a presence that signals you are not available to
receive stanzas.

 -}
unavailable : List Node -> Presence
unavailable nodes = presence Nothing Unavailable nodes


{-| Construct an `unavailable` presence stanza.

Use this when you want to send an `unavailable` presence to a specific
entity instead of broadcasting through the server.

 -}
unavailableTo : JID -> List Node -> Presence
unavailableTo to nodes = presence (Just to) Unavailable nodes


{-| Construct a `subscribe` presence stanza.

Use this to construct a presence that signals your intent to subscribe
to a specific entity.

 -}
subscribe : JID -> List Node -> Presence
subscribe to nodes = presence (Just to) Subscribe nodes


{-| Construct a `subscribed` presence stanza.

Use this to respond to a `subscribe` stanza directed at your entity,
if the subscription was successfully added.

 -}
subscribed : JID -> List Node -> Presence
subscribed to nodes = presence (Just to) Subscribed nodes


{-| Construct a `unsubscribe` presence stanza.

Use this to construct a presence that signals your intent to subscribe
to a specific entity.

 -}
unsubscribe : JID -> List Node -> Presence
unsubscribe to nodes = presence (Just to) Unsubscribe nodes


{-| Construct a `unsubscribed` presence stanza.

Use this to respond to an `unsubscribe` stanza directed at your
entity, if the subscription was successfully removed.

 -}
unsubscribed : JID -> List Node -> Presence
unsubscribed to nodes = presence (Just to) Unsubscribed nodes


{-| Construct a `probe` presence stanza.

You probably won't need this, but if you do, you'll know.

 -}
probe : JID -> List Node -> Presence
probe to nodes = presence (Just to) Probe nodes


{-| Construct a `error` presence stanza.

You probably won't need this, but if you do, you'll know.

 -}
error : JID -> List Node -> Presence
error to nodes = presence (Just to) Error nodes


------------------------------------------------------------
-- Inspecting a Presence Stanza
------------------------------------------------------------

inspect : Presence -> (Attributes, List Node)
inspect (Presence attrs nodes) = (attrs, nodes)


------------------------------------------------------------
-- Encoding to XML
------------------------------------------------------------

showType : Type -> String
showType type_ =
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
        attrs = Utils.filterAttributes
                [ Attribute "to"       (fromJID to)
                , Attribute "from"     (fromJID from)
                , Attribute "type"     (showType type_)
                , Attribute "id"       (toString id)
                , Attribute "xml:lang" (toString xmllang)
                , Attribute "xmlns"    "jabber:client"
                ]
    in
        Element "presence" attrs nodes


------------------------------------------------------------
-- Decoding from XML
------------------------------------------------------------


{-| Decode a Presence stanza from an XML Node.

 -}
fromXml : Node -> Result String Presence
fromXml node =
    case node of
        Element "presence" attrs children ->
            Ok (Presence (toAttributes attrs) children)
        _ ->
            Err "invalid presence stanza"

toAttributes : List Attribute -> Attributes
toAttributes =
    List.foldl updateAttribute def

updateAttribute : Attribute -> Attributes -> Attributes
updateAttribute { name, value } attrs =
    case name of
        "to" ->
            Utils.updateAttributeTo value attrs

        "from" ->
            Utils.updateAttributeFrom value attrs

        "type" ->
            updateAttributeType value attrs

        "id" ->
            Utils.updateAttributeId value attrs

        "xml:lang" ->
            Utils.updateAttributeXmllang value attrs

        _ ->
            attrs

updateAttributeType : String -> { r | type_ : Type } -> { r | type_ : Type }
updateAttributeType value attrs =
    case readType value of
        Just type_ ->
            { attrs | type_ = type_ }

        Nothing ->
            attrs

readType : String -> Maybe Type
readType s =
    case s of
        ""             -> Just Available
        "unavailable"  -> Just Unavailable
        "subscribe"    -> Just Subscribe
        "subscribed"   -> Just Subscribed
        "unsubscribe"  -> Just Unsubscribe
        "unsubscribed" -> Just Unsubscribed
        "probe"        -> Just Probe
        "error"        -> Just Error
        _              -> Nothing

