module Client exposing (..)

import Websocket
import Json.Decode as JD
import Json.Encode as JE
import Task
import Xmpp.Connection
import Xmpp.Xml exposing (Xml)
import Xmpp.Sasl

toCmd : msg -> Cmd msg
toCmd = Task.perform identity << Task.succeed


type alias Model =
    { socketInfo : Maybe Websocket.ConnectionInfo
    , credentials : Credentials
    , sasl : SaslAuth
    }


type ExternalMsg
    = Message String
    | None


type Credentials
    = Credentials String String

initialModel : Credentials -> Model
initialModel creds =
    { socketInfo = Nothing
    , credentials = creds
    , sasl = NotStarted
    }

type alias Config =
    { url : String
    , credentials : Credentials
    }

init : Config -> (Model, Cmd Msg)
init { url, credentials } =
    ( initialModel credentials
    , Websocket.connect url [ "xmpp" ])



type Msg
    = SocketConnected Websocket.ConnectionInfo
    | ReceivedString String
    | SendString String
    | Error String
    | GotSaslInitialResponse JD.Value
    | GotSaslChallengeResponse JD.Value
    | GotSaslSuccessValidation JD.Value

type SaslAuth
    = NotStarted
    | OpenSent
    | OpenReceived
    | FeaturesReceived
    | AuthSent JD.Value
    | ChallengeReceived
    | ChallengeResponseSent JD.Value
    | AuthSuccessReceived
    | AuthCompleted


update : Msg -> Model -> (Model, Cmd Msg, ExternalMsg)
update msg model =
    case Debug.log "DEBUG: " msg of
        SocketConnected socketInfo ->
            ({ model
                 | socketInfo = Just socketInfo
                 , sasl = OpenSent
             }
            , sendXml Xmpp.Connection.open
            , None)

        SendString s ->
            case model.socketInfo of
                Just socketInfo ->
                    ( model
                    , Websocket.sendString socketInfo s
                    , None
                    )

                Nothing ->
                    (model, Cmd.none, None)

        ReceivedString message ->
            case Xmpp.Connection.decodeResponse message of
                Ok resp ->
                    let _ = Debug.log "Decode Response Ok" (resp, model.sasl) in
                    handleResponse resp model

                Err e ->
                    Debug.log "Decode Response Failed"
                    ( model
                    , Cmd.none
                    , None
                    )

        Error errMsg ->
            let _ = Debug.log "Error" errMsg in
            (model, Cmd.none, None)

        GotSaslInitialResponse data ->
            case decodeSaslInitialResponse data of
                Ok token ->
                    if model.sasl == FeaturesReceived then
                        ({ model | sasl = AuthSent data }
                        , sendInitialResponse token
                        , None
                        )
                    else
                        (model, Cmd.none, None)

                Err e ->
                    let _ = Debug.log "Decoding Failed: " e in
                    (model, Cmd.none, None)


        GotSaslChallengeResponse data ->
            case decodeSaslChallengeResponse data of
                Ok token ->
                    if model.sasl == ChallengeReceived then
                        Debug.log "ChallengeResponseSent"
                        ({ model | sasl = ChallengeResponseSent data }
                        , sendChallengeResponse token
                        , None
                        )
                    else
                        Debug.log "Challenge Failed" (model, Cmd.none, None)

                Err e ->
                    let _ = Debug.log "Decoding Failed: " e in
                    (model, Cmd.none, None)

        GotSaslSuccessValidation data ->
            case decodeSaslSuccessValidation data of
                Ok isValid ->
                    if model.sasl == AuthSuccessReceived && isValid then
                        Debug.log "SASL Success"
                        ({ model | sasl = AuthCompleted }
                        , Cmd.none
                        , None
                        )
                    else
                        Debug.log "Success Validation Failed" (model, Cmd.none, None)

                Err e ->
                    let _ = Debug.log "Decoding Failed: " e in
                    (model, Cmd.none, None)


handleResponse : Xmpp.Connection.Response
               -> Model
               -> (Model, Cmd Msg, ExternalMsg)
handleResponse resp model =
    case resp of
        Xmpp.Connection.OpenResponse id ->
            let _ = Debug.log "OpenResponse" id in
            if model.sasl == OpenSent then
                ({ model | sasl = OpenReceived }
                , Cmd.none
                , None)
            else
                (model, Cmd.none, None)

        Xmpp.Connection.FeaturesResponse mechanisms ->
            if model.sasl == OpenReceived -- || model.sasl == OpenSent
            then
                ({ model | sasl = FeaturesReceived }
                , Websocket.sendCmd "sasl-initial"
                    (encodeCredentials model.credentials)
                , None)
            else
                (model, Cmd.none, None)

        Xmpp.Connection.ChallengeResponse serverMessage ->
            let _ = Debug.log "ChallengeResponse" model.sasl in
            case model.sasl of
                AuthSent data ->
                    ({ model | sasl = ChallengeReceived }
                    , Websocket.sendCmd "sasl-challenge"
                        (encodeChallenge serverMessage data)
                    , None)

                _ ->
                    (model, Cmd.none, None)

        Xmpp.Connection.AuthSuccess successMessage ->
            let _ = Debug.log "AuthSuccess" model.sasl in
            case model.sasl of
                ChallengeResponseSent data ->
                    ({ model | sasl = AuthSuccessReceived }
                    , Websocket.sendCmd "sasl-validate-success"
                        (encodeSuccess successMessage data)
                    , None)

                _ ->
                    (model, Cmd.none, None)


subscriptions : Model -> Sub Msg
subscriptions model =
    Websocket.events
        (\event ->
            case event of
                Websocket.Connected info ->
                    SocketConnected info

                Websocket.StringMessage message ->
                    ReceivedString message

                Websocket.SaslInitialResponse data ->
                    GotSaslInitialResponse data

                Websocket.SaslChallengeResponse data ->
                    GotSaslChallengeResponse data

                Websocket.SaslSuccessValidation data ->
                    GotSaslSuccessValidation data

                Websocket.BadMessage error ->
                    Error error
        )


sendInitialResponse : String -> Cmd Msg
sendInitialResponse data =
    sendXml <|
        Xmpp.Connection.auth data Xmpp.Sasl.SCRAM_SHA_1

decodeSaslInitialResponse : JD.Value -> Result JD.Error String
decodeSaslInitialResponse value =
    JD.decodeValue (JD.field "encoded" JD.string) value


decodeSaslChallengeResponse : JD.Value -> Result JD.Error String
decodeSaslChallengeResponse value =
    JD.decodeValue (JD.field "encoded" JD.string) value

sendChallengeResponse : String -> Cmd Msg
sendChallengeResponse data =
    sendXml <|
        Xmpp.Connection.challengeResponse data

decodeSaslSuccessValidation : JD.Value -> Result JD.Error Bool
decodeSaslSuccessValidation = JD.decodeValue JD.bool

send : String -> Cmd Msg
send = toCmd << SendString << Debug.log "DEBUG: "

sendXml : Xml -> Cmd Msg
sendXml = send << Xmpp.Xml.format


encodeCredentials : Credentials -> JE.Value
encodeCredentials (Credentials username password )=
    JE.object
        [ ("username", JE.string username)
        , ("password", JE.string password)
        ]

encodeChallenge : String -> JE.Value -> JE.Value
encodeChallenge serverMessage clientData =
    JE.object
        [ ("serverFirstMessage", JE.string serverMessage)
        , ("client", clientData)
        ]

encodeSuccess : String -> JE.Value -> JE.Value
encodeSuccess successMessage challengeData =
    JE.object
        [ ("successMessage", JE.string successMessage)
        , ("challengeData", challengeData)
        ]

