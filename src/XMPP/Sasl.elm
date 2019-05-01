module Xmpp.Sasl
    exposing
    ( Mechanism(..)
    , showMechanism
    , mechanismDecoder
    )

import Xmpp.Xml.Decode as XD


type Mechanism
    = SCRAM_SHA_1
    -- | SCRAM_SHA_1_PLUS
    | PLAIN
    -- | EXTERNAL

mechanismDecoder : XD.Decoder Mechanism
mechanismDecoder =
    XD.string |> XD.andThen (\s ->
        case s of
            "SCRAM-SHA-1" -> XD.succeed SCRAM_SHA_1
            -- "SCRAM-SHA-1-PLUS" -> XD.succeed SCRAM_SHA_1_PLUS
            "PLAIN" -> XD.succeed PLAIN
            -- "EXTERNAL" -> XD.succeed EXTERNAL
            _ -> XD.fail "unsupported mechanism"
    )

showMechanism : Mechanism -> String
showMechanism m =
    case m of
        SCRAM_SHA_1 -> "SCRAM-SHA-1"
        -- SCRAM_SHA_1_PLUS -> "SCRAM-SHA-1-PLUS"
        PLAIN -> "PLAIN"
        -- EXTERNAL -> "EXTERNAL"


        
