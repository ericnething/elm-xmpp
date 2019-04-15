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
    )

import XmlParser exposing (..)
import XMPP.Stanza.Utils exposing (filterAttributes)
import XMPP.JID as JID exposing (JID)

{- See page 105 of https://datatracker.ietf.org/doc/rfc6120/ -}

type Iq = Iq Attributes (List Node)

type alias Attributes =
    { to      : Maybe JID
    , from    : Maybe JID
    , type_   : Type
    , id      : Maybe String -- REQUIRED (will be added by connection manager)
    , xmllang : Maybe String
    }

type Type
    = Get
    | Set
    | Result
    | Error


-- Constructing

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

get : List Node -> Iq
get nodes = iq Nothing Get nodes

getTo : JID -> List Node -> Iq
getTo to nodes = iq (Just to) Get nodes

set : List Node -> Iq
set nodes = iq Nothing Set nodes

setTo : JID -> List Node -> Iq
setTo to nodes = iq (Just to) Set nodes

result : JID -> List Node -> Iq
result to nodes = iq (Just to) Result nodes

error : JID -> List Node -> Iq
error to nodes = iq (Just to) Error nodes


-- Inspecting

inspect : Iq -> (Attributes, List Node)
inspect (Iq attr nodes) = (attr, nodes)


-- XML

encodeType : Type -> String
encodeType type_ =
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
        attrs = filterAttributes
                [ { name = "to", value = fromJID to }
                , { name = "from", value = fromJID from }
                , { name = "type", value = encodeType type_ }
                , { name = "id", value = toString id }
                , { name = "xml:lang", value = toString xmllang }
                , { name = "xmlns", value = "jabber:client" }
                ]
    in
        Element "iq" attrs nodes
