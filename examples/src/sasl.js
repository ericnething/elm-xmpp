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


// Test case
// scramSha1.challengeResponse({ serverFirstMessage: btoa("r=fyko+d2lbbFgONRv9qkxdawL3rfcNHYJY1ZVvWVs7j,s=QSXCR+Q6sek8bf92,i=4096"), client: scramSha1.initialResponse({username: "user", password: "pencil"}, "fyko+d2lbbFgONRv9qkxdawL")}).then(console.log)


const [encode64, decode64] = (function(){
  "use strict";

  var chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

  // Use a lookup table to find the index.
  var lookup = new Uint8Array(256);
  for (var i = 0; i < chars.length; i++) {
    lookup[chars.charCodeAt(i)] = i;
  }

  const encode = function(arraybuffer) {
    var bytes = new Uint8Array(arraybuffer),
    i, len = bytes.length, base64 = "";

    for (i = 0; i < len; i+=3) {
      base64 += chars[bytes[i] >> 2];
      base64 += chars[((bytes[i] & 3) << 4) | (bytes[i + 1] >> 4)];
      base64 += chars[((bytes[i + 1] & 15) << 2) | (bytes[i + 2] >> 6)];
      base64 += chars[bytes[i + 2] & 63];
    }

    if ((len % 3) === 2) {
      base64 = base64.substring(0, base64.length - 1) + "=";
    } else if (len % 3 === 1) {
      base64 = base64.substring(0, base64.length - 2) + "==";
    }

    return base64;
  };

  const decode =  function(base64) {
    var bufferLength = base64.length * 0.75,
    len = base64.length, i, p = 0,
    encoded1, encoded2, encoded3, encoded4;

    if (base64[base64.length - 1] === "=") {
      bufferLength--;
      if (base64[base64.length - 2] === "=") {
        bufferLength--;
      }
    }

    var arraybuffer = new ArrayBuffer(bufferLength),
    bytes = new Uint8Array(arraybuffer);

    for (i = 0; i < len; i+=4) {
      encoded1 = lookup[base64.charCodeAt(i)];
      encoded2 = lookup[base64.charCodeAt(i+1)];
      encoded3 = lookup[base64.charCodeAt(i+2)];
      encoded4 = lookup[base64.charCodeAt(i+3)];

      bytes[p++] = (encoded1 << 2) | (encoded2 >> 4);
      bytes[p++] = ((encoded2 & 15) << 4) | (encoded3 >> 2);
      bytes[p++] = ((encoded3 & 3) << 6) | (encoded4 & 63);
    }

    return arraybuffer;
  };

    return [encode, decode]
})();


function base64ArrayBuffer(arrayBuffer) {
  var base64    = ''
  var encodings = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'

  var bytes         = new Uint8Array(arrayBuffer)
  var byteLength    = bytes.byteLength
  var byteRemainder = byteLength % 3
  var mainLength    = byteLength - byteRemainder

  var a, b, c, d
  var chunk

  // Main loop deals with bytes in chunks of 3
  for (var i = 0; i < mainLength; i = i + 3) {
    // Combine the three bytes into a single integer
    chunk = (bytes[i] << 16) | (bytes[i + 1] << 8) | bytes[i + 2]

    // Use bitmasks to extract 6-bit segments from the triplet
    a = (chunk & 16515072) >> 18 // 16515072 = (2^6 - 1) << 18
    b = (chunk & 258048)   >> 12 // 258048   = (2^6 - 1) << 12
    c = (chunk & 4032)     >>  6 // 4032     = (2^6 - 1) << 6
    d = chunk & 63               // 63       = 2^6 - 1

    // Convert the raw binary segments to the appropriate ASCII encoding
    base64 += encodings[a] + encodings[b] + encodings[c] + encodings[d]
  }

  // Deal with the remaining bytes and padding
  if (byteRemainder == 1) {
    chunk = bytes[mainLength]

    a = (chunk & 252) >> 2 // 252 = (2^6 - 1) << 2

    // Set the 4 least significant bits to zero
    b = (chunk & 3)   << 4 // 3   = 2^2 - 1

    base64 += encodings[a] + encodings[b] + '=='
  } else if (byteRemainder == 2) {
    chunk = (bytes[mainLength] << 8) | bytes[mainLength + 1]

    a = (chunk & 64512) >> 10 // 64512 = (2^6 - 1) << 10
    b = (chunk & 1008)  >>  4 // 1008  = (2^6 - 1) << 4

    // Set the 2 least significant bits to zero
    c = (chunk & 15)    <<  2 // 15    = 2^4 - 1

    base64 += encodings[a] + encodings[b] + encodings[c] + '='
  }
  
  return base64
}


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
        for (let k = 1; k <= iterations - 1; ++k) {
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
    initialResponse: function({ username, password }, nonce0) {
        const nonce = nonce0 || _bufferToHex(window.crypto.getRandomValues(new Uint8Array(32)));
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
                    salt: decode64(salt),
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
        
        const authMessage = _appendBuffer(
            _appendBuffer(
                scramSha1.textEncoder.encode(`${client.initialMessageBare},`),
                decode64(serverFirstMessage)),
            scramSha1.textEncoder.encode(`,${clientFinalMessageBare}`)
        );

        const clientSignature = await HMAC(storedKey, authMessage);
        // const clientProof = String.fromCharCode.apply(
        //     null, _xor(clientKey, clientSignature)
        // );
        const clientProof = _xor(clientKey, clientSignature);
        console.log("clientProof", clientProof);
        console.log("clientProof encoded", base64ArrayBuffer(clientProof));
        console.log("clientProof encode64", encode64(clientProof));
        const serverKey = await HMAC(saltedPassword, scramSha1.textEncoder.encode("Server Key"));
        const serverSignature = await HMAC(serverKey, authMessage);
        console.log("Server Signature", base64ArrayBuffer(serverSignature));
        const clientFinalMessage = `${clientFinalMessageBare},p=${base64ArrayBuffer(clientProof)}`;
        return btoa(clientFinalMessage);
    }
}
