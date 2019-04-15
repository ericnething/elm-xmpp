module XMPP.Stanza
    exposing
    ( encode
    , json
    )

import XmlParser exposing (..)


encode : Node -> String
encode stanza = format <| Xml [] Nothing <| stanza



{-| JSON containers as defined in [XEP-0335][xep-335].

[xep-335]: https://xmpp.org/extensions/xep-0335.html
 -}
json : String -> Node
json s =
    Element "json" [ Attribute "xmlns" "urn:xmpp:json:0" ] [ Text s ]

