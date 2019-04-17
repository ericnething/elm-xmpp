module Xmpp.Stanza.Utils exposing (..)

import Xmpp.Xml exposing (..)
import Xmpp.JID as JID exposing (JID)

filterAttributes : List Attribute -> List Attribute
filterAttributes =
    List.filter (not << String.isEmpty << .value)

updateAttributeTo : String
                  -> { r | to : Maybe JID }
                  -> { r | to : Maybe JID }
updateAttributeTo value attrs =
    { attrs | to = JID.fromString value }

updateAttributeFrom : String
                    -> { r | from : Maybe JID }
                    -> { r | from : Maybe JID }
updateAttributeFrom value attrs =
    { attrs | from = JID.fromString value }

updateAttributeId : String
                  -> { r | id : Maybe String }
                  -> { r | id : Maybe String }
updateAttributeId value attrs =
    if not (String.isEmpty value) then
        { attrs | id = Just value }
    else
        attrs

updateAttributeXmllang : String
                       -> { r | xmllang : Maybe String }
                       -> { r | xmllang : Maybe String }
updateAttributeXmllang value attrs =
    if not (String.isEmpty value) then
        { attrs | xmllang = Just value }
    else
        attrs
