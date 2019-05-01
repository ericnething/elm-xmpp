"use strict";


/*
     SaltedPassword  := Hi(Normalize(password), salt, i)
     ClientKey       := HMAC(SaltedPassword, "Client Key")
     StoredKey       := H(ClientKey)
     AuthMessage     := client-first-message-bare + "," +
                        server-first-message + "," +
                        client-final-message-without-proof
     ClientSignature := HMAC(StoredKey, AuthMessage)
     ClientProof     := ClientKey XOR ClientSignature
     ServerKey       := HMAC(SaltedPassword, "Server Key")
     ServerSignature := HMAC(ServerKey, AuthMessage)
*/

const saslprep = function(string) {
    return string;
}

const _appendBuffer = function(buffer1, buffer2) {
    let tmp = new Uint8Array(buffer1.byteLength + buffer2.byteLength);
    tmp.set(new Uint8Array(buffer1), 0);
    tmp.set(new Uint8Array(buffer2), buffer1.byteLength);
    return tmp.buffer;
}

const _xor = function(buffer1, buffer2) {
    const result = new Uint8Array(buffer1.length);
    for (let k = 0; k < buffer1.length; ++k) {
        result[k] = buffer1[k] ^ buffer2[k];
    }
}

const _bufferToHex = buffer =>
      Array.prototype.map.call(
          new Uint8Array(buffer),
          x => ('00' + x.toString(16)).slice(-2)
      ).join('');

const scramSha1 = {
    textEncoder: new TextEncoder("utf-8"),
    textDecoder: new TextDecoder("utf-8"),
    
    Hi: function(password, salt, iterations) {
        const HMAC = scramSha1.HMAC;
        const int1 = new Uint8Array([0, 0, 0, 1]);
        
        let ui1 = HMAC(text, _appendBuffer(salt, int1));
        let ui = ui1;
        for (let k = 0; k < iterations - 1; k++) {
            ui1 = HMAC(text, ui1);
            ui = _xor(ui, ui1);
        }

        return ui;
    },
    HMAC: async function(key, data) {
        const key = await window.crypto.subtle.importKey(
            "raw",
            key,
            { name: "HMAC", hash: { name: "SHA-1" } },
            false,
            ["sign", "verify"]
        );
        const hash = await window.crypto.subtle.sign(
            { name: "HMAC" },
            key, //from generateKey or importKey above
            data //ArrayBuffer of data you want to sign
        );
        return new Uint8Array(hash);
    },
    H: async function(data) {
        // returns the hash as an ArrayBuffer
        const hash = await window.crypto.subtle.digest(
            { name: "SHA-1" },
            // The data you want to hash as an ArrayBuffer
            data
        );
        return hash;
    },
    initialResponse: function({ username, password }) {
        const nonce = _bufferToHex(window.crypto.getRandomValues(new Uint8Array(32)));
        const normalizedPassword = saslprep(password);
        const initialMessageBare = `n=${username},r=${nonce}`;
        const encoded = btoa(`n,,${initialMessage}`);

        return {
            initialMessageBare,
            encoded,
            normalizedPassword: scramSha1.textEncoder.encode(normalizedPassword)
        }
    },
    challengeResponse: function(serverFirstMessage, client) {
        const textEncode = scramSha1.textEncoder.encode;
        const textDecode = scramSha1.textDecoder.decode;
        const Hi = scramSha1.Hi;
        const H = scramSha1.H;
        const HMAC = scramSha1.HMAC;
        
        const decodeServerChallenge = function(text) {
            const [[r, serverNonce],
                   [s, salt],
                   [i, iterations]
                  ] = text.split(",").map(a => a.split("="));
            if ((r === "r" && !serverNonce) &&
                (s === "s" && !salt) &&
                (i === "i" && !iterations)) {
                return {
                    serverNonce,
                    salt: textEncode(salt),
                    iterations: parseInt(iterations)
                }
            }
        };

        const { serverNonce, salt, iterations } = decodeServerChallenge(serverFirstMessage);
        
        const clientFinalMessageBare = `c=biws,r=${serverNonce}`;
        
        const saltedPassword = Hi(client.normalizedPassword, salt, iterations);
        const clientKey = HMAC(saltedPassword, textEncode("Client Key"));
        const storedKey = H(clientKey);
        
        const authMessage = textEncode([
            client.initialMessageBare,
            serverFirstMessage,
            clientFinalMessageBare
        ].join(","));

        const clientSignature = HMAC(storedKey, authMessage);
        const clientProof = btoa(textDecode(_xor(clientKey, clientSignature)));
        const serverKey = HMAC(saltedPassword, textEncode("Server Key"));
        const serverSignature = HMAC(serverKey, authMessage);

        const clientFinalMessage = `${clientFinalMessageBare},p=${clientProof}`;
        return clientFinalMessage;
    }
}
