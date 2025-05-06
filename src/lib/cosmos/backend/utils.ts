import {
    Bip39,
    EnglishMnemonic,
    Random,
    Secp256k1,
    Slip10,
    Slip10Curve,
    stringToPath,
} from "@cosmjs/crypto";
import { entropyToMnemonic } from "@cosmjs/crypto/build/bip39";
import { Ed25519Signature2020 } from "@digitalbazaar/ed25519-signature-2020";
import { Ed25519VerificationKey2020 } from "@digitalbazaar/ed25519-verification-key-2020";
import {
    issue,
    derive,
    verify,
    verifyCredential,
    createPresentation,
    signPresentation,
} from "@digitalbazaar/vc";
import * as ed25519 from "@noble/ed25519";
import { hmac } from "@noble/hashes/hmac";
import { sha256 } from "@noble/hashes/sha256";
import { concatBytes } from "@noble/hashes/utils";
import { etc, sign } from "@noble/secp256k1";
import { HDKey } from "@scure/bip32";
import { exec } from "child_process";
import { ec as EC } from "elliptic";
import { readFile, unlink, writeFile } from "fs/promises";
import * as jsonld from "jsonld";
import { base58btc } from "multiformats/bases/base58";
import { tmpdir } from "os";
import { join } from "path";
import { canonize } from "rdf-canonize";
import { promisify } from "util";
import { schemaMap } from "./schemas/schemaMap";
import { getDocumentLoader } from "./utils-jsonld-nquads";
import * as Multikey from "@digitalbazaar/ed25519-multikey";
import { driver as DidKeyDriver } from "@digitalbazaar/did-method-key";
import { DataIntegrityProof } from "@digitalbazaar/data-integrity";
import { generateMnemonic, mnemonicToSeedSync } from "bip39";
import { hdkey } from "ethereumjs-wallet";
import { Buffer } from "buffer";
import * as ed from "@noble/ed25519";

const execAsync = promisify(exec);

export async function getRawPrivkeyFromMnemonic(
    mnemonic: string
): Promise<Uint8Array> {
    const hdPath = stringToPath("m/44'/118'/0'/0/0");
    const seed = await Bip39.mnemonicToSeed(new EnglishMnemonic(mnemonic));
    const { privkey } = Slip10.derivePath(Slip10Curve.Secp256k1, seed, hdPath);
    return privkey;
}

/**
 * Derives a Multikey-compatible key from a BIP39 mnemonic using Digital Bazaar's generate()
 */
export async function deriveMultikeyFromMnemonic(
    mnemonic: string
): Promise<any> {
    const fullSeed = mnemonicToSeedSync(mnemonic); // 64 bytes
    const seed = fullSeed.slice(0, 32); // Only use first 32 bytes for Ed25519

    return await Multikey.generate({ seed });
}

/**
 * Derives an Ed25519 JWK key pair from a BIP39 mnemonic
 */
export async function deriveEd25519JwkFromMnemonic(mnemonic: string): Promise<{
    publicJwk: { kty: string; crv: string; x: string };
    privateJwk: { kty: string; crv: string; x: string; d: string };
}> {
    const seed = mnemonicToSeedSync(mnemonic);
    const node = HDKey.fromMasterSeed(seed).derive("m/44'/0'/0'/0/0");

    const privateKey = node.privateKey!;
    const publicKey = await ed25519.getPublicKey(privateKey);

    const d = Buffer.from(privateKey).toString("base64url");
    const x = Buffer.from(publicKey).toString("base64url");

    return {
        publicJwk: { kty: "OKP", crv: "Ed25519", x },
        privateJwk: { kty: "OKP", crv: "Ed25519", x, d },
    };
}

async function generateEd25519KeysFromMnemonic(
    mnemonic: string,
    path: string = "m/44'/0'/0'/0/0"
): Promise<{ publicKey: Uint8Array; privateKey: Uint8Array }> {
    const seed = mnemonicToSeedSync(mnemonic);
    const root = hdkey.fromMasterSeed(seed);
    const child = root.derivePath(path);
    const privateKey = child.getWallet().getPrivateKey();

    // Ensure private key is 32 bytes
    const paddedPrivateKey =
        privateKey.length < 32
            ? Buffer.concat([Buffer.alloc(32 - privateKey.length), privateKey])
            : privateKey.slice(0, 32);

    const publicKey = await ed.getPublicKey(paddedPrivateKey);
    return { publicKey, privateKey: paddedPrivateKey };
}

/**
 * Derives a full Ed25519VerificationKey2020-compatible key pair from mnemonic
 */
export async function deriveKeyPairForSignature2020(mnemonic: string): Promise<{
    id: string;
    controller: string;
    publicKeyMultibase: string;
    privateKeyMultibase: string;
}> {
    const seed = mnemonicToSeedSync(mnemonic);
    const node = HDKey.fromMasterSeed(seed).derive("m/44'/0'/0'/0/0");

    const privateKey = node.privateKey!;
    const publicKey = await ed25519.getPublicKey(privateKey); // 32 bytes

    // Apply multicodec prefix for Ed25519: 0xED
    const ed25519Prefix = new Uint8Array([0xed]);
    const publicKeyWithPrefix = concatBytes(ed25519Prefix, publicKey);
    const privateKeyMultibase = base58btc.encode(privateKey);
    const publicKeyMultibase = base58btc.encode(publicKeyWithPrefix);

    const controller = `did:key:${publicKeyMultibase}`;
    const id = `${controller}#${publicKeyMultibase}`;

    return {
        id,
        controller,
        publicKeyMultibase,
        privateKeyMultibase,
    };
}

/**
 * Generates a valid random did:key using HD derivation with secp256k1
 * @returns {Promise<string>} A valid DID like `did:key:zQ3...`
 */

export async function generateRandomDidKey(): Promise<string> {
    // 1. Generate random mnemonic
    const mnemonic = entropyToMnemonic(Random.getBytes(16)); // Generates a 12-word mnemonic by default

    // 2. Derive seed and private key
    const hdPath = stringToPath("m/44'/118'/0'/0/0");
    const seed = await Bip39.mnemonicToSeed(new EnglishMnemonic(mnemonic));
    const { privkey } = Slip10.derivePath(Slip10Curve.Secp256k1, seed, hdPath);

    if (!privkey) throw new Error("Failed to derive private key");

    // 3. Get compressed public key
    const uncompressed = (await Secp256k1.makeKeypair(privkey)).pubkey;
    const pubkey = Secp256k1.compressPubkey(uncompressed); // ✅ compressed = 33 bytes

    // 4. Add multicodec prefix 0xE7 (secp256k1-pub) and encode
    const multicodecPrefix = Uint8Array.from([0xe7]);
    const prefixedKey = concatBytes(multicodecPrefix, pubkey);
    const encoded = base58btc.encode(prefixedKey); // starts with z...

    // 5. Build DID
    return `did:key:${encoded}`;
}

/**
 * Signs a credential using the axoned CLI + jsonld CLI
 * @param credential The JSON-LD credential to sign
 * @param issuer_wallet_name The name of the issuer wallet
 * @returns Signed credential in N-Quads format
 */
export async function signCredentialWithCLI(
    credential: any,
    issuer_wallet_name: string
): Promise<string> {
    const tmpJsonld = join(tmpdir(), `credential-${Date.now()}.jsonld`);
    const tmpNquads = join(tmpdir(), `credential-${Date.now()}.nq`);

    const AXONED_PATH =
        process.env.AXONED_PATH ||
        "/home/manuelpadilla/sources/reposUbuntu/AXONE/tools/axoned-10.0.0-linux-amd64/axoned";
    const KEYRING_BACKEND =
        process.env.KEYRING_BACKEND || "--keyring-backend os";
    try {
        // Save credential to temp file
        await writeFile(tmpJsonld, JSON.stringify(credential, null, 2));
        console.log(`[signCredentialViaCLI] Wrote credential to: ${tmpJsonld}`);

        // Build the command
        const cmd = `${AXONED_PATH} credential sign ${tmpJsonld} ${KEYRING_BACKEND} --from ${issuer_wallet_name} | jsonld toRdf -q - > ${tmpNquads}`;
        console.log(`[signCredentialViaCLI] Running command:\n${cmd}`);

        // Run command via shell
        await execAsync(cmd, { shell: "/bin/bash" });
        console.log(
            `[signCredentialViaCLI] Signature completed, reading N-Quads...`
        );

        // Read resulting N-Quads
        const nquads = await readFile(tmpNquads, "utf-8");
        console.log(`[signCredentialViaCLI] Read ${nquads.length} bytes`);

        return nquads;
    } catch (err) {
        console.error(`[signCredentialViaCLI] Error during CLI signing:`, err);
        throw new Error("Credential signing via CLI failed");
    } finally {
        // Clean up
        await unlink(tmpJsonld).catch(() => {});
        await unlink(tmpNquads).catch(() => {});
    }
}

/**
 * Signs a credential using the wallet and rdf-canonize for URDNA2015 normalization
 * @param credential Credential to sign
 * @param mnemonic Mnemonic for the wallet
 * @returns Signed credential with proof
 */
export async function signCredentialWithElliptic(
    credential: any,
    mnemonic: string
): Promise<any> {
    try {
        console.log(`[signCredential] Init`);

        const documentToSign = { ...credential };
        delete documentToSign.proof;

        const privKey = await getRawPrivkeyFromMnemonic(mnemonic);

        // Convert to N-Quads (TS)
        const nquads = await jsonld.toRDF(documentToSign, {
            format: "application/n-quads",
            documentLoader: getDocumentLoader(schemaMap),
        });

        // Canonicalize with rdf-canonize (TS)
        const canonicalized = await canonize(nquads, {
            algorithm: "URDNA2015",
            inputFormat: "application/n-quads",
        });

        const messageHash = sha256(new TextEncoder().encode(canonicalized));

        const ec = new EC("secp256k1");

        function bigIntTo32Bytes(bn: any): Uint8Array {
            const hex = bn.toString(16).padStart(64, "0");
            return Uint8Array.from(
                hex.match(/.{2}/g)!.map((byte: string) => parseInt(byte, 16))
            );
        }

        function signCompactECDSA(
            hash: Uint8Array,
            privKey: Uint8Array
        ): Uint8Array {
            const key = ec.keyFromPrivate(privKey);
            const sig = key.sign(hash, { canonical: true });

            const r = bigIntTo32Bytes(sig.r);
            const s = bigIntTo32Bytes(sig.s);
            return new Uint8Array([...r, ...s]); // compact 64-byte signature
        }

        const signature = signCompactECDSA(messageHash, privKey);
        const signatureBase64 = Buffer.from(signature).toString("base64url");

        const header = {
            alg: "unknown",
            b64: false,
            crit: ["b64"],
        };
        const encodedHeader = Buffer.from(JSON.stringify(header)).toString(
            "base64url"
        );

        const jws = `${encodedHeader}..${signatureBase64}`;

        const didKeyParts = credential.issuer.id.split(":");
        const keyFragment = didKeyParts[didKeyParts.length - 1];
        const verificationMethod = `${credential.issuer.id}#${keyFragment}`;

        const now = new Date();
        const tzOffset = now.getTimezoneOffset() * -1;
        const tzHours = String(Math.floor(Math.abs(tzOffset) / 60)).padStart(
            2,
            "0"
        );
        const tzMinutes = String(Math.abs(tzOffset) % 60).padStart(2, "0");
        const tzSign = tzOffset >= 0 ? "+" : "-";
        const microseconds =
            String(now.getMilliseconds()).padStart(3, "0") + "000";
        const timestamp = `${now
            .toISOString()
            .replace("Z", "")
            .substring(0, 19)}.${microseconds}${tzSign}${tzHours}:${tzMinutes}`;

        const proof = {
            type: "EcdsaSecp256k1Signature2019",
            created: timestamp,
            proofPurpose: "assertionMethod",
            verificationMethod,
            jws,
        };

        return {
            ...credential,
            proof,
        };
    } catch (error) {
        console.error(`[signCredential] Error:`, error);
        throw new Error(
            `Failed to sign credential: ${
                error instanceof Error ? error.message : String(error)
            }`
        );
    }
}

/**
 * Signs a credential using the wallet
 * This function creates a proper JWS signature according to the Axone protocol requirements
 * @param credential Credential to sign
 * @param mnemonic Mnemonic for the wallet
 * @returns Signed credential
 */
export async function signCredentialWithNoble(
    credential: any,
    mnemonic: string
): Promise<any> {
    try {
        console.log(`[signCredential] Init`);

        // Clone the credential object and remove any existing proof
        const documentToSign = { ...credential };
        delete documentToSign.proof;
        console.log(
            `[signCredential] Prepared document for signing: ${JSON.stringify(
                documentToSign
            )}`
        );

        // Get the private key
        const privKey = await getRawPrivkeyFromMnemonic(mnemonic);

        // Convert the document to JSON-LD Normalized form (canonicalization)
        // First convert to n-quads format
        console.log(
            `[signCredential] Converting to N-Quads for canonicalization`
        );
        let canonicalizedData;
        try {
            // Use a more complete JSON-LD library if available
            // This is a simplification - ideally use something compatible with Hyperledger Aries
            canonicalizedData = await jsonld.normalize(documentToSign, {
                algorithm: "URDNA2015",
                format: "application/n-quads",
                documentLoader: getDocumentLoader(schemaMap),
            });
            console.log(
                `[signCredential] Successfully canonicalized document: ${canonicalizedData}`
            );
        } catch (error) {
            throw new Error(
                `[signCredential] Error during canonicalization: ${error}`
            );
        }

        // Hash the canonicalized document using SHA-256
        const messageHash = sha256(new TextEncoder().encode(canonicalizedData));
        console.log(
            `[signCredential] Document hashed (${messageHash.length} bytes)`
        );

        // Sign the hash
        etc.hmacSha256Sync = (key, msgs) => hmac(sha256, key, msgs); // required by noble
        const signatureBytes = await sign(messageHash, privKey, {
            lowS: true,
            extraEntropy: true,
        }).toCompactRawBytes();

        // Create JWS header and detached signature in the format Axone expects
        const header = {
            alg: "unknown",
            b64: false,
            crit: ["b64"],
        };
        const encodedHeader = Buffer.from(JSON.stringify(header)).toString(
            "base64url"
        );
        const signatureBase64 =
            Buffer.from(signatureBytes).toString("base64url");
        const jws = `${encodedHeader}..${signatureBase64}`;

        console.log(
            `[signCredential] Detached JWS created: ${jws.substring(0, 20)}...`
        );

        // Build the DID verification method
        const didKeyParts = credential.issuer.id.split(":");
        const keyFragment = didKeyParts[didKeyParts.length - 1];
        const verificationMethod = `${credential.issuer.id}#${keyFragment}`;

        console.log(
            `[signCredential] Verification method: ${verificationMethod}`
        );

        // Format timestamp to match CLI output (RFC3339 format)
        // The CLI uses the local timezone offset instead of 'Z'
        // Format the timestamp similar to the CLI output
        // CLI format includes microseconds and timezone offset
        const now = new Date();
        const tzOffset = now.getTimezoneOffset() * -1;
        const tzHours = String(Math.floor(Math.abs(tzOffset) / 60)).padStart(
            2,
            "0"
        );
        const tzMinutes = String(Math.abs(tzOffset) % 60).padStart(2, "0");
        const tzSign = tzOffset >= 0 ? "+" : "-";

        // Format with microseconds precision (pad with zeros if needed)
        const microseconds =
            String(now.getMilliseconds()).padStart(3, "0") + "000";

        // Format the timestamp in the specific format the CLI uses
        const timestamp = `${now
            .toISOString()
            .replace("Z", "")
            .substring(0, 19)}.${microseconds}${tzSign}${tzHours}:${tzMinutes}`;

        // Create the proof object in the exact format expected by Axone
        const proof = {
            type: "EcdsaSecp256k1Signature2019",
            created: timestamp,
            proofPurpose: "assertionMethod",
            verificationMethod: verificationMethod,
            jws: jws,
        };

        console.log(`[signCredential] Proof built: ${JSON.stringify(proof)}`);

        // Return the signed credential
        return {
            ...credential,
            proof,
        };
    } catch (error) {
        console.error(`[signCredential] Error:`, error);
        throw new Error(
            `Failed to sign credential: ${
                error instanceof Error ? error.message : String(error)
            }`
        );
    }
}

/**
 * Signs a Verifiable Credential using a mnemonic, by deriving an Ed25519 JWK key and using Ed25519Signature2020.
 * @param credential Unsigned credential (JSON-LD)
 * @param mnemonic BIP39 mnemonic to derive the key
 * @returns Signed Verifiable Credential (JSON-LD with proof)
 */
export async function signCredentialWithJwk(
    credential: Record<string, unknown>,
    mnemonic: string
): Promise<Record<string, unknown>> {
    try {
        // Derive JWK key pair from mnemonic
        // Convert JWK to Multikey format
        const keyPair = await deriveMultikeyFromMnemonic(mnemonic);

        // Initialize DID Key driver
        const didKeyDriver = DidKeyDriver();

        didKeyDriver.use({
            multibaseMultikeyHeader: "z6Mk",
            fromMultibase: Multikey.from,
        });

        // Generate DID Document from key pair
        const { didDocument } = await didKeyDriver.fromKeyPair({
            verificationKeyPair: keyPair,
        });

        // Set key pair ID and controller
        keyPair.id = didDocument.assertionMethod[0];
        keyPair.controller = didDocument.id;

        // Set issuer and issuance date
        credential.issuer = keyPair.controller;
        credential.issuanceDate = new Date().toISOString();

        // Create Data Integrity Proof suite
        // const suite = new DataIntegrityProof({
        //     signer: keyPair.signer(),
        //     cryptosuite: "eddsa-2022",
        //     date: new Date(),
        // });

        const suite = new Ed25519Signature2020({
            key: keyPair,
            date: new Date(),
        });

        // Sign the credential
        const signedCredential = await issue({
            credential,
            suite,
            documentLoader: getDocumentLoader(schemaMap),
        });

        return signedCredential;
    } catch (error) {
        console.error(`[signCredentialWithJwk] Error:`, error);
        throw new Error(
            `Failed to sign credential with JWK: ${
                error instanceof Error ? error.message : String(error)
            }`
        );
    }
}
/**
 * Formats a file size for display
 * @param bytes Size in bytes
 * @returns Human-readable size representation
 */
export function formatFileSize(bytes: number): string {
    if (bytes < 1024) return bytes + " bytes";
    else if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(2) + " KB";
    else if (bytes < 1024 * 1024 * 1024)
        return (bytes / (1024 * 1024)).toFixed(2) + " MB";
    else return (bytes / (1024 * 1024 * 1024)).toFixed(2) + " GB";
}
