module XMPP.Stanza.Iq
    exposing
    -- Types
    ( Iq
    , Attributes
    , Type

    -- Construct
    , get
    , getTo
    , set
    , setTo
    , result
    , error

    -- Inspect
    , inspect

    -- XML
    , toXml
    , fromXml
    )

{-| Iq

# Types

@doc Iq, Attributes, Type

# Constructing Iq Stanzas

@doc get, getTo, set, setTo, result, error

# Inspecting a Iq Stanza

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

type Iq = Iq Attributes (List Node)

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
    , type_   = Get
    , id      = Nothing
    , xmllang = Nothing
    }

{- See page 105 of https://datatracker.ietf.org/doc/rfc6120/ -}

type Type
    = Get
    | Set
    | Result
    | Error


------------------------------------------------------------
-- Constructing Iq Stanzas
------------------------------------------------------------

{-| Generic iq constuctor meant for internal use only.

 -}
iq : Maybe JID -> Type -> List Node -> Iq
iq to type_ nodes =
    Iq
    { to      = to
    , from    = Nothing
    , type_   = type_
    , id      = Nothing
    , xmllang = Nothing
    }
    nodes


{-| Construct a `get` iq stanza.

Use this to construct a request for data from the server. Think of
this like an HTTP GET request.

 -}
get : List Node -> Iq
get nodes = iq Nothing Get nodes


{-| Construct a `get` iq stanza.

Similar to `get`, but you can direct the request to a specific entity.

 -}
getTo : JID -> List Node -> Iq
getTo to nodes = iq (Just to) Get nodes


{-| Construct a `set` iq stanza.

Use this to construct a request to write data to the server. Think of
this like an HTTP POST request.

 -}
set : List Node -> Iq
set nodes = iq Nothing Set nodes


{-| Construct a `set` iq stanza.

Similar to `set`, but you can direct the request to a specific entity.

 -}
setTo : JID -> List Node -> Iq
setTo to nodes = iq (Just to) Set nodes


{-| Construct a `result` iq stanza.

Use this to construct a response to a `get` or `set` iq stanza
directed at you.

 -}
result : JID -> List Node -> Iq
result to nodes = iq (Just to) Result nodes


{-| Construct an `error` iq stanza.

Use this to construct an error response to any iq stanza directed at
you.

 -}
error : JID -> List Node -> Iq
error to nodes = iq (Just to) Error nodes


------------------------------------------------------------
-- Inspecting an Iq Stanza
------------------------------------------------------------

inspect : Iq -> (Attributes, List Node)
inspect (Iq attr nodes) = (attr, nodes)


------------------------------------------------------------
-- Encoding to XML
------------------------------------------------------------

showType : Type -> String
showType type_ =
    case type_ of
        Get    -> "get"
        Set    -> "set"
        Result -> "result"
        Error  -> "error"


toXml : Iq -> Node
toXml (Iq { to, from, type_, id, xmllang } nodes) =
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
        Element "iq" attrs nodes


------------------------------------------------------------
-- Decoding from XML
------------------------------------------------------------


{-| Decode an Iq stanza from an XML Node.

 -}
fromXml : Node -> Result String Iq
fromXml node =
    case node of
        Element "iq" attrs children ->
            Ok (Iq (toAttributes attrs) children)
        _ ->
            Err "invalid iq stanza"

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
        "get"    -> Just Get
        "set"    -> Just Set
        "result" -> Just Result
        "error"  -> Just Error
        _        -> Nothing

