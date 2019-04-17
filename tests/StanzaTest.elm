module StanzaTest exposing (testDiceRoll)

import Expect
import Test exposing (..)
import Xmpp.Xml exposing (..)
import Xmpp.Stanza.Message as Message
import Xmpp.JID as JID exposing (JID)
import Xmpp.Stanza exposing (json)
import Xmpp.Xml.Decode as XD


alice = JID.new "alice" "wonderland.lit" ""
rabbit = JID.new "rabbit" "wonderland.lit" ""

testDiceRoll : Test
testDiceRoll =
    describe "diceroll decoding test"
        [ test "json dice roll" <| \_ ->
              let
                  msg = Message.chat rabbit
                        [ Message.body "Hello"
                        , Element "diceroll"
                            [ Attribute "xmlns" "roll2d6.org/diceroll" ]
                            [ json """{ "dicetype": "fate" }""" ]
                        ]
                  decoder _ =
                      XD.succeed "yay!"
              in
                  Expect.equal
                      (Message.decode decoder msg)
                      (Ok "yay!")
        , test "xml fate dice roll" <| \_ ->
              let
                  msg = sampleFateDiceRoll
                  decoder = diceRollDecoder
                  result =
                      { from = JID.new "alice" "wonderland.lit" ""
                      , request = "4dF"
                      , type_ = FateDice
                      , result = DFate DFatePlus DFateMinus DFateBlank DFatePlus
                      }
              in
                  Expect.equal
                      (Debug.log "result" (Message.decode decoder msg))
                      (Ok result)
        , test "xml number dice roll" <| \_ ->
              let
                  msg = sampleNumberDiceRoll
                  decoder = diceRollDecoder
                  result =
                      { from = JID.new "alice" "wonderland.lit" ""
                      , request = "2d20"
                      , type_ = NumberDice
                      , result = DNumber [DNumberFace 20 8, DNumberFace 20 17]
                      }
              in
                  Expect.equal
                      (Debug.log "result" (Message.decode decoder msg))
                      (Ok result)

        , test "xml redmarkets dice roll" <| \_ ->
              let
                  msg = sampleRedMarketsDiceRoll
                  decoder = diceRollDecoder
                  result =
                      { from = JID.new "alice" "wonderland.lit" ""
                      , request = "rm + 2"
                      , type_ = RedMarketsDice
                      , result = DRedMarkets { red = 6, black = 5 }
                      }
              in
                  Expect.equal
                      (Debug.log "result" (Message.decode decoder msg))
                      (Ok result)
        ]

-- filterNodes : String -> List Node -> List Node
-- filterNodes name nodes =
--     let
--         predicate node =
--             case node of
--                 Element tag attrs children ->
--                     tag == name
--                 _ ->
--                     False
--     in
--         List.filter predicate nodes

-- path : List String -> Node -> Result String (List Node)
-- path names node =
--     let
--         step name acc =
--             case Result.map (filterNodes name) acc of
--                 Ok nodes ->
--                     case nodes of
--                         [ Element _ attrs children ] ->
--                             Ok children
--                         _ ->
--                             Err ("Element named " ++ name ++ " not found.")
--                 _ ->
--                     Err ("Element named " ++ name ++ " not found.")
--     in
--         List.foldl step (Ok [ node ]) names

type alias DiceRoll =
    { from : JID
    , request : String
    , type_ : DiceType
    , result : Dice
    }

type DiceType
    = FateDice
    | NumberDice
    | RedMarketsDice

type Dice
    = DFate DFateFace DFateFace DFateFace DFateFace
    | DNumber (List DNumberFace)
    | DRedMarkets DRedMarketsFace

type alias DNumberFace =
    { max : Int
    , value : Int
    }

type alias DRedMarketsFace =
    { red : Int
    , black : Int
    }

type DFateFace
    = DFatePlus
    | DFateMinus
    | DFateBlank

diceTypeDecoder : XD.Decoder DiceType
diceTypeDecoder =
    XD.stringAttr "type" |> XD.andThen (\s ->
        case s of
            "fate" -> XD.succeed FateDice
            "polyhedral" -> XD.succeed NumberDice
            "redmarkets" -> XD.succeed RedMarketsDice
            _ -> XD.fail (s ++ " is not a valid dice type.")
    )

dFateDecoder : XD.Decoder DFateFace
dFateDecoder =
    XD.string |> XD.andThen (\s ->
        case s of
            "+" -> XD.succeed DFatePlus
            "-" -> XD.succeed DFateMinus
            "b" -> XD.succeed DFateBlank
            _ -> XD.fail (s ++ " is not a valid fate dice face.")
    )

dNumberDecoder : XD.Decoder DNumberFace
dNumberDecoder =
    XD.map2 DNumberFace
        (XD.intAttr "max")
        (XD.int)

-- dRedMarketsDecoder : XD.Decoder DRedMarketsFace
-- dRedMarketsDecoder =
--     XD.map2 DRedMarketsFace
--         (XD.intAttr "red")
--         (XD.int)
            

sampleFateDiceRoll =
        Message.chat rabbit
            [ Message.body "Hello"
            , Element "diceroll"
                [ Attribute "xmlns" "roll2d6.org/diceroll"
                , Attribute "type" "fate"
                , Attribute "request" "4dF"
                ]
                [ Element "dice" [ Attribute "type" "fate" ] [ Text "+" ]
                , Element "dice" [ Attribute "type" "fate" ] [ Text "-" ]
                , Element "dice" [ Attribute "type" "fate" ] [ Text "b" ]
                , Element "dice" [ Attribute "type" "fate" ] [ Text "+" ]
                ]
            ]
            |> Message.setFrom alice

sampleNumberDiceRoll =
        Message.chat rabbit
            [ Message.body "Hello"
            , Element "diceroll"
                [ Attribute "xmlns" "roll2d6.org/diceroll"
                , Attribute "type" "polyhedral"
                , Attribute "request" "2d20"
                ]
                [ Element "dice"
                      [ Attribute "type" "polyhedral"
                      , Attribute "max" "20"
                      ]
                      [ Text "8" ]
                , Element "dice"
                    [ Attribute "type" "polyhedral"
                    , Attribute "max" "20"
                    ]
                    [ Text "17" ]
                ]
            ]
            |> Message.setFrom alice

sampleRedMarketsDiceRoll =
        Message.chat rabbit
            [ Message.body "Hello"
            , Element "diceroll"
                [ Attribute "xmlns" "roll2d6.org/diceroll"
                , Attribute "type" "redmarkets"
                , Attribute "request" "rm + 2"
                ]
                [ Element "dice"
                      [ Attribute "type" "redmarkets"
                      , Attribute "color" "red"
                      ]
                      [ Text "6" ]
                , Element "dice"
                      [ Attribute "type" "redmarkets"
                      , Attribute "color" "blue"
                      ]
                      [ Text "5" ]
                ]
            ]
            |> Message.setFrom alice

diceRollDecoder : Message.Attributes -> XD.Decoder DiceRoll
diceRollDecoder { from } =
    XD.path [ XD.tag "diceroll" ] <| XD.single <|
        XD.map4 DiceRoll
            (jidDecoder from)
            (XD.stringAttr "request")
            diceTypeDecoder
            diceResultDecoder

jidDecoder : Maybe JID -> XD.Decoder JID
jidDecoder mjid =
    case mjid of
        Just jid -> XD.succeed jid
        Nothing -> XD.fail "JID is missing"

diceResultDecoder : XD.Decoder Dice
diceResultDecoder =
    diceTypeDecoder |> XD.andThen (\diceType ->
        case diceType of
            FateDice ->
                XD.map4 DFate
                    (XD.path [XD.tag "dice"] (XD.index 0 dFateDecoder))
                    (XD.path [XD.tag "dice"] (XD.index 1 dFateDecoder))
                    (XD.path [XD.tag "dice"] (XD.index 2 dFateDecoder))
                    (XD.path [XD.tag "dice"] (XD.index 3 dFateDecoder))

            NumberDice ->
                XD.map DNumber
                    (XD.path [XD.tag "dice"] (XD.list dNumberDecoder))

            RedMarketsDice ->
                XD.map2 (\a b -> DRedMarkets <| DRedMarketsFace a b)
                    (XD.path
                         [XD.tagWith "dice" [Attribute "color" "red"]]
                         (XD.single XD.int))
                    (XD.path
                         [XD.tagWith "dice" [Attribute "color" "black"]]
                         (XD.single XD.int))
    )
