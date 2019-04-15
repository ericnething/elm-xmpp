{-

outgoing      <open>
incoming      <open>
incoming      <features>
outgoing      <auth>
incoming      <challenge>
outgoing      <response>
incoming      <success>
outgoing      <open>
incoming      <open>
incoming      <features>
outgoing      <iq>
incoming      <iq>
outgoing      <enable>
incoming      <enabled>
outgoing      <r sm>
outgoing      <iq xmpp-session>
incoming      <a sm>


 -}

module XMPP exposing (..)

import XMPP.JID as JID exposing (JID)
import XMPP.Stanza as Stanza exposing (Stanza)

hello : String
hello = "Hello"
