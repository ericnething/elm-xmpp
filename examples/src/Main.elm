module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, pre, text)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)
import Http
import Client



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { client : Client.Model
    , toSend : String
    , sentMessages : List String
    , receivedMessages : List String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        (client, cmd) =
            Client.init
            { url = "ws://localhost:5280/ws-xmpp"
            , credentials = Client.Credentials "welkin" "foobar"
            }
    in
        ( { client = client
          , toSend = """<open xmlns="urn:ietf:params:xml:ns:xmpp-framing" xml:lang="en" to="localhost" version="1.0"/>"""
          , sentMessages = []
          , receivedMessages = []
          }
        , Cmd.map ClientMsg cmd
        )



-- UPDATE

type Msg
    = ClientMsg Client.Msg
    | UpdateInput String
    | SendString


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClientMsg clientMsg ->
            let
                (newClient, cmd, externalMsg) =
                    Client.update clientMsg model.client

                (updatedModel, externalCmd) =
                    handleClientMsg externalMsg { model | client = newClient}
            in
                ( updatedModel
                , Cmd.batch
                    [ Cmd.map ClientMsg cmd
                    , externalCmd
                    ]
                )

        UpdateInput string ->
            ( { model | toSend = string }, Cmd.none )

        SendString ->
            ( { model | sentMessages = model.toSend :: model.sentMessages }
            , Cmd.map ClientMsg (Client.send model.toSend)
            )


handleClientMsg : Client.ExternalMsg -> Model -> (Model, Cmd Msg)
handleClientMsg msg model =
    case msg of
        Client.None ->
            (model, Cmd.none)

        Client.Message message ->
            ({ model | receivedMessages = message :: model.receivedMessages }
            , Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ClientMsg (Client.subscriptions model.client)


-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ connectionState model
        , stringMsgControls model
        ]


connectionState model =
    div []
        [ case model.client.socketInfo of
            Nothing ->
                text "Connecting..."

            Just info ->
                div []
                    [ text "Connected to "
                    , text info.url
                    ]
        ]


stringMsgControls : Model -> Html Msg
stringMsgControls model =
    div []
        [ div []
            [ button [ onClick SendString ] [ text "Send" ]
            , input [ onInput UpdateInput, value model.toSend ] []
            ]
        , div []
            [ div [ class "sent" ]
                (div [] [ text "Sent" ]
                    :: List.map messageInfo model.sentMessages
                )
            , div []
                (div [] [ text "Received" ]
                    :: List.map messageInfo model.receivedMessages
                )
            ]
        ]


messageInfo : String -> Html Msg
messageInfo message =
    div [] [ text message ]
