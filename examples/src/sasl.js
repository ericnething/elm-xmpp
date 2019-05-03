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
    const result = new Uint8Array(buffer1.byteLength);
    for (let k = 0; k < buffer1.byteLength; ++k) {
        result[k] = buffer1[k] ^ buffer2[k];
    }
    return result
}

const _bufferToHex = buffer =>
      Array.prototype.map.call(
          new Uint8Array(buffer),
          x => ('00' + x.toString(16)).slice(-2)
      ).join('');

const scramSha1 = {
    textEncoder: new TextEncoder("utf-8"),
    textDecoder: new TextDecoder("utf-8"),
    
    Hi: async function(password, salt, iterations) {
        const HMAC = scramSha1.HMAC;
        const int1 = new Uint8Array([0, 0, 0, 1]);
        
        let ui1 = await HMAC(password, _appendBuffer(salt, int1));
        // console.log("first HMAC round");
        let ui = ui1;
        for (let k = 0; k < iterations - 1; k++) {
            // console.log(`HMAC iteration ${k}`);
            ui1 = await HMAC(password, ui1);
            ui = _xor(ui, ui1);
        }

        return ui;
    },
    HMAC: async function(key, data) {
        const hmackey = await window.crypto.subtle.importKey(
            "raw",
            key,
            { name: "HMAC", hash: { name: "SHA-1" } },
            false,
            ["sign", "verify"]
        );
        const hash = await window.crypto.subtle.sign(
            { name: "HMAC" },
            hmackey, //from generateKey or importKey above
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
        const encoded = btoa(`n,,${initialMessageBare}`);

        return {
            nonce,
            initialMessageBare,
            encoded,
            normalizedPassword: scramSha1.textEncoder.encode(normalizedPassword)
        }
    },
    challengeResponse: async function({ serverFirstMessage, client }) {
        const Hi = scramSha1.Hi;
        const H = scramSha1.H;
        const HMAC = scramSha1.HMAC;
        
        const decodeServerChallenge = function(text) {
            const isNonemptyString = s => typeof s == "string" && s.length > 0;
            const [r_serverNonce, s_salt, i_iterations] = atob(text).split(",");
            const serverNonce =
                  r_serverNonce.startsWith("r=") && r_serverNonce.slice(2);
            
            const salt = s_salt.startsWith("s=") && s_salt.slice(2);
            
            const iterations =
                  i_iterations.startsWith("i=") && i_iterations.slice(2);

            if ((isNonemptyString(serverNonce) &&
                     serverNonce.startsWith(client.nonce)) &&
                isNonemptyString(salt) &&
                isNonemptyString(iterations)) {
                console.log(serverNonce, salt, iterations);
                return {
                    serverNonce,
                    salt: scramSha1.textEncoder.encode(salt),
                    iterations: parseInt(iterations)
                }
            } else {
                console.log("Decoding of challenge response failed.");
            }
        };

        const { serverNonce, salt, iterations } = decodeServerChallenge(serverFirstMessage);
        
        const clientFinalMessageBare = `c=biws,r=${serverNonce}`;
        
        const saltedPassword = await Hi(client.normalizedPassword, salt, iterations);
        const clientKey = await HMAC(saltedPassword, scramSha1.textEncoder.encode("Client Key"));
        const storedKey = await H(clientKey);
        
        const authMessage = scramSha1.textEncoder.encode([
            client.initialMessageBare,
            serverFirstMessage,
            clientFinalMessageBare
        ].join(","));

        const clientSignature = await HMAC(storedKey, authMessage);
        // const clientProof = String.fromCharCode.apply(
        //     null, _xor(clientKey, clientSignature)
        // );
        const clientProof = _xor(clientKey, clientSignature);
        console.log("clientProof", clientProof);
        const serverKey = await HMAC(saltedPassword, scramSha1.textEncoder.encode("Server Key"));
        const serverSignature = await HMAC(serverKey, authMessage);
        const clientFinalMessage = `${clientFinalMessageBare},p=${btoa(clientProof)}`;
        return btoa(clientFinalMessage);
    }
}
