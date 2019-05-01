module Xmpp.Stanza.Iq
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

    -- Modify Attributes
    , setId
    , setXmllang
 
    -- Decode
    , decode

    -- XML
    , toXml
    , fromXml
    )

{-| Iq

# Types

@doc Iq, Attributes, Type

# Constructing Iq Stanzas

@doc get, getTo, set, setTo, result, error

# Modify the Attributes

@doc setId, setXmllang

# Decode into your own custom type

@doc decode

# Convert to XML

@doc toXml

# Convert from XML

@doc fromXml

 -}

import Xmpp.Xml exposing (..)
import Xmpp.Stanza.Utils as Utils
import Xmpp.JID as JID exposing (JID)
import Xmpp.Xml.Decode as XD

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
-- Modify the Attributes
------------------------------------------------------------

{- | Set the id attribute.

The id attribute for iq stanzas is required. If you don't provide one,
the connection manager will add one for you before sending your
message out. Use this if you would like to track your iq stanzas with
a unique identifier.

    Iq.get
        [ Element "query" [ Attribute "xmlns" "jabber:iq:roster" ] [] ]
    |> Iq.setId "a1b2c3"

 -}
setId : String -> Iq -> Iq
setId id (Iq attrs nodes) =
    Iq { attrs | id = Just id } nodes

{- | Set the xml:lang attribute.

Use this to set the default language that should be used to interpret
any textual data in your content. If you don't set this attribute,
your server will set it for you to its own default.

    Iq.result alice
        [ Element "query"
            [ Attribute "xmlns" "http://jabber.org/protocol/disco#items" ]
            [ Element "item" [ Attribute "jid" "muc.wonderland.lit" ] []
            , Element "item" [ Attribute "jid" "teaparty.wonderland.lit" ] []
            ]
        ]
    |> Iq.setXmllang "en"

 -}
setXmllang : String -> Iq -> Iq
setXmllang xmllang (Iq attrs nodes) =
    Iq { attrs | xmllang = Just xmllang } nodes


------------------------------------------------------------
-- Decode into your own custom type
------------------------------------------------------------

{-| Get the attributes of an Iq stanza.

 -}
attributes : Iq -> Attributes
attributes (Iq attrs nodes) = attrs

{-| Get the children nodes of an Iq stanza.

 -}
children : Iq -> List Node
children (Iq attrs nodes) = nodes


decode : (Attributes -> XD.Decoder a) -> Iq -> Result XD.Error a
decode toDecoder msg =
    XD.decodeXml (toDecoder (attributes msg)) (toXml msg)

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


toXml : Iq -> Xml
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
        Xml [] Nothing <|
            Element "iq" attrs nodes


------------------------------------------------------------
-- Decoding from XML
------------------------------------------------------------


{-| Decode an Iq stanza from an XML Node.

 -}
fromXml : Xml -> Result String Iq
fromXml doc =
    case doc.root of
        Element "iq" attrs childNodes ->
            Ok (Iq (toAttributes attrs) childNodes)
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

