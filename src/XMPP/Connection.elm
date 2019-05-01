module Xmpp.Connection exposing (..)

{-


[xmpp-core]: https://xmpp.org/rfcs/rfc6120.html
[sasl]: https://tools.ietf.org/html/rfc4422
[sasl-mechanisms]: https://www.iana.org/assignments/sasl-mechanisms/sasl-mechanisms.xhtml

 -}

import Xmpp.Xml exposing (Xml, Node(..), Attribute)
import Xmpp.Stanza
import Xmpp.JID as JID exposing (JID)
import Xmpp.Xml.Decode as XD
import Xmpp.Sasl as Sasl

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
outgoing      <enable sm>
incoming      <enabled sm>
outgoing      <r sm>
outgoing      <iq xmpp-session>
incoming      <a sm>
 -}

type alias Config =
    { credentials : Credentials
    , wsUrl : String
    , useStreamManagement : Bool
    }

type Credentials
    = PlainText JID String
    | Token String


type Response
    = OpenResponse String
    | FeaturesResponse (List Sasl.Mechanism)

-- connect : String -> Cmd msg
-- connect url =
--     Websocket.connect "ws://localhost:5280/ws-xmpp" ["xmpp"]



ns =
    { framing = "urn:ietf:params:xml:ns:xmpp-framing"
    , sasl = "urn:ietf:params:xml:ns:xmpp-sasl"
    }

{-| Wrap a stanza in a root element to we can easily decode it using
the `path` function from Xmpp.Xml.Decode

 -}
wrapStanza : Xml -> Xml
wrapStanza xmldoc =
    { xmldoc | root = Element "root" [] [ xmldoc.root ] }


{-| Convert a Node into a proper Xml document

 -}
toXml : Node -> Xml
toXml = Xml [] Nothing


{- Initialize the stream

   C:  <open xmlns="urn:ietf:params:xml:ns:xmpp-framing"
             to="example.com"
             version="1.0" />

   S:  <open xmlns="urn:ietf:params:xml:ns:xmpp-framing"
             from="example.com"
             id="++TR84Sm6A3hnt3Q065SnAbbk3Y="
             xml:lang="en"
             version="1.0" />

 -}

open : Xml
open =
    toXml <|
        Element "open"
            [ Attribute "xmlns" ns.framing
            , Attribute "to" "localhost"
            , Attribute "version" "1.0"
            ]
        []

openResponseDecoder : XD.Decoder Response
openResponseDecoder =
    XD.map OpenResponse <|
        XD.path
            [XD.tagWith "open"
                 [ Attribute "xmlns" ns.framing
                 , Attribute "from" "localhost"
                 , Attribute "version" "1.0"
                 ]
            ]
            (XD.single (XD.stringAttr "id"))


{-

S: <features>
     <mechanisms xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>
       <mechanism>EXTERNAL</mechanism>
       <mechanism>SCRAM-SHA-1-PLUS</mechanism>
       <mechanism>SCRAM-SHA-1</mechanism>
       <mechanism>PLAIN</mechanism>
     </mechanisms>
   </features>

 -}

featuresResponseDecoder : XD.Decoder Response
featuresResponseDecoder =
    XD.map FeaturesResponse <|
        XD.path
            [ XD.tag "features"
            , XD.tagWith "mechanisms" [ Attribute "xmlns" ns.sasl ]
            , XD.tag "mechanism"
            ]
            (XD.leakyList Sasl.mechanismDecoder)


auth : Sasl.Mechanism -> Xml
auth mech =
    toXml <|
        Element "auth"
            [ Attribute "xmlns" ns.sasl
            , Attribute "mechanism" (Sasl.showMechanism mech)
            ]
            []

