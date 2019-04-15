module XMPP.JID
    exposing
    ( JID
    , fromString
    , toString
    , toBare
    , local
    , domain
    , resource
    , new
    )

import Parser
    exposing
    ( Parser
    , symbol
    , oneOf
    , (|.)
    , (|=)
    , succeed
    , getChompedString
    , chompUntilEndOr
    , chompUntil
    , end
    , problem
    , andThen
    , run
    )

type JID = JID
    { local : String
    , domain : String
    , resource : String
    }

toBare : JID -> JID
toBare (JID jid) =
    JID { jid | resource = "" }

local : JID -> String
local (JID jid) = jid.local

domain : JID -> String
domain (JID jid) = jid.domain

resource : JID -> String
resource (JID jid) = jid.resource

toString : JID -> String
toString (JID jid) =
    let
        localPart =
            if String.isEmpty jid.local
            then ""
            else jid.local ++ "@"
        resourcePart =
            if String.isEmpty jid.resource
            then ""
            else "/" ++ jid.resource
    in
        localPart ++ jid.domain ++ resourcePart

fromString : String -> Maybe JID
fromString = Result.toMaybe << run parser
    

new : String -> String -> String -> JID
new localPart domainPart resourcePart =
    JID { local    = localPart
        , domain   = domainPart
        , resource = resourcePart
        }

parser : Parser JID
parser =
    succeed new
        |= localParser
        |= domainParser
        |= resourceParser

localParser : Parser String
localParser =
    oneOf
    [ getChompedString (succeed () |. chompUntil "@") |. symbol "@"
    , succeed ""
    ]

domainParser : Parser String
domainParser =
    oneOf
    [ getChompedString (succeed () |. chompUntil "/") |. symbol "/"
    , getChompedString (succeed () |. chompUntilEndOr " ") |. end
    ] |> andThen checkDomain

resourceParser : Parser String
resourceParser =
    getChompedString (succeed () |. chompUntilEndOr " ")

checkDomain : String -> Parser String
checkDomain dom =
    if String.isEmpty dom then
        problem "a JID must at least have a domain"
    else
        succeed dom
    
