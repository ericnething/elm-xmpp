"use strict";

const bind = function(app) {
    // Exit predictably if the ports aren't in use in Elm
    if (!app.ports || !(app.ports.toSocket && app.ports.fromSocket)) {
        console.log(
            "Could not find 'toSocket' and 'fromSocket' ports on app. They may not be in use yet."
        );
        return;
    }

    let sockets = {};

    // Handle events from Elm
    app.ports.toSocket.subscribe(message => {
        console.log("port message", message);
        switch (message.msgType) {
        case "connect":
            openWebsocket(message.msg);
            break;
        case "send-string":
            sendString(message.msg);
            break;
        case "sasl-initial": {
            const resp = scramSha1.initialResponse(message.msg);
            app.ports.fromSocket.send({
                msgType: "sasl-initial-response",
                msg: resp
            });
        }
            break;
        case "sasl-challenge":
            scramSha1.challengeResponse(message.msg)
                .then(resp => app.ports.fromSocket.send({
                    msgType: "sasl-challenge-response",
                    msg: resp
                }));
            break;
        case "sasl-validate-success": {
            const resp = scramSha1.validateSuccess(message.msg);
            app.ports.fromSocket.send({
                msgType: "sasl-success-validated",
                msg: resp
            });
        }
        }
    });

    function openWebsocket(request) {
        if (sockets[request.url]) {
            console.log(
                `There's already an open socket for ${request.url}, ignoring request.`
            );
            return;
        }
        let toElm = app.ports.fromSocket;
        let socket = new WebSocket(request.url, request.protocols);

        socket.onopen = openHandler.bind(null, toElm, socket, request.url);
        socket.onmessage = messageHandler.bind(null, toElm, request.url);
        socket.onerror = errorHandler.bind(null, toElm, request.url);
        socket.onclose = closeHandler.bind(null, toElm, sockets, request.url);

        sockets[request.url] = socket;
    }

    function sendString(request) {
        let socket = sockets[request.url];
        if (socket) {
            socket.send(request.message);
        } else {
            console.log(
                `No open socket for: ${request.url}. Cannot send ${request.message}`
            );
        }
    }
}

function openHandler(toElm, socket, url, event) {
    toElm.send({
        msgType: "connected",
        msg: {
            url: url,
            binaryType: socket.binaryType,
            extensions: socket.extensions,
            protocol: socket.protocol
        }
    });
}

function messageHandler(toElm, url, event) {
    if (typeof event.data == "string") {
        toElm.send({
            msgType: "string-message",
            msg: event.data
        });
    }
}

function errorHandler(toElm, url, event) {
    toElm.send({
        msgType: "error",
        msg: {
            url: url,
            code: event.code
        }
    });
}

function closeHandler(toElm, sockets, url, event) {
    let socket = sockets[url];
    sockets[url] = undefined;

    toElm.send({
        msgType: "closed",
        msg: {
            url: url,
            unsentBytes: socket.bufferedAmount
        }
    });
}
