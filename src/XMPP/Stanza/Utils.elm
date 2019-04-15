module XMPP.Stanza.Utils exposing (..)

import XmlParser exposing (..)

filterAttributes : List Attribute -> List Attribute
filterAttributes =
    List.filter (not << String.isEmpty << .value)
