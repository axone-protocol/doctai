import {
    AXONE_NODE_RPC,
    DATAVERSE_ADDR,
    MINIO_DID,
    MINIO_WALLET_MNEMONIC_PHRASE,
} from "@/lib/constants";
import {
    CosmWasmClient,
    SigningCosmWasmClient,
} from "@cosmjs/cosmwasm-stargate";
import {
    Bip39,
    EnglishMnemonic,
    Random,
    Secp256k1,
    Slip10,
    Slip10Curve,
    stringToPath,
} from "@cosmjs/crypto";
import { DirectSecp256k1HdWallet } from "@cosmjs/proto-signing";
import { GasPrice } from "@cosmjs/stargate";
import { hmac } from "@noble/hashes/hmac";
import { sha256 } from "@noble/hashes/sha256";
import { concatBytes } from "@noble/hashes/utils";
import { etc, sign } from "@noble/secp256k1";
import {
    fetchGovAddressFromDID,
    fetchZoneDIDFromMinIODID,
} from "../frontend/axone";
import { getDocumentLoader } from "./normalizeJsonLd";
import { schemaMap } from "./schemas/schemaMap";
import { entropyToMnemonic } from "@cosmjs/crypto/build/bip39";
import { base58btc } from "multiformats/bases/base58";

import { writeFile, readFile, unlink } from "fs/promises";
import { exec } from "child_process";
import { tmpdir } from "os";
import { join } from "path";
import { promisify } from "util";

import jsonld from "jsonld";
import { canonize } from "rdf-canonize";
import { ec as EC } from "elliptic";

const execAsync = promisify(exec);

/**
 * Checks if a user is a contributor by querying the zone governance contract
 * @param zone_gov_code_address The address of the zone governance contract
 * @param userDID The DID of the user to check
 * @returns True if the user is a contributor, false otherwise
 */
export async function queryIsContributor(
    zone_gov_code_address: string,
    userDID: string
): Promise<boolean> {
    try {
        console.log(`[queryIsContributor] Starting check for DID: ${userDID}`);
        const client = await CosmWasmClient.connect(AXONE_NODE_RPC);
        console.log(
            `[queryIsContributor] Connected to Axone node: ${AXONE_NODE_RPC}`
        );

        const query = {
            ask: {
                query: `tell('${userDID}', 'resource:register', Result, Evidence).`,
            },
        };

        console.log(
            `[queryIsContributor] Querying zone contract: ${zone_gov_code_address}`
        );
        console.log(
            `[queryIsContributor] Query payload: ${JSON.stringify(query)}`
        );

        const response = await client.queryContractSmart(
            zone_gov_code_address,
            query
        );
        console.log(
            `[queryIsContributor] Response received: ${JSON.stringify(
                response
            )}`
        );

        const substitutions =
            response?.answer?.results?.[0]?.substitutions ?? [];

        const resultSubstitution = substitutions.find(
            (s: any) => s.variable === "Result"
        );

        const isPermitted = resultSubstitution?.expression === "permitted";

        console.log(
            `[queryIsContributor] Result for DID ${userDID}: permitted=${isPermitted}`
        );

        return isPermitted;
    } catch (error) {
        console.error(
            "[queryIsContributor] Failed to check contributor permission:",
            error
        );
        return false;
    }
}

/**
 * Checks if a user has permission to upload a file
 * @param userDID The DID of the user
 * @param file The file to upload
 * @param txHash Transaction hash for payment verification (optional)
 * @returns True if the user has permission to upload the file, false otherwise
 */
export async function checkUploadPermission(
    userDID: string,
    file: File,
    txHash: string | null = null
): Promise<boolean> {
    try {
        console.log("[checkUploadPermission] Starting permission check");
        console.log("[checkUploadPermission] User DID:", userDID);
        console.log("[checkUploadPermission] File info:", {
            name: file.name,
            size: file.size,
            type: file.type,
        });

        const zoneDID = await fetchZoneDIDFromMinIODID(MINIO_DID);
        console.log("[checkUploadPermission] Fetched zone DID:", zoneDID);
        if (!zoneDID) throw new Error("Zone DID not found from MINIO_DID");

        const zoneGovAddress = await fetchGovAddressFromDID(zoneDID);
        console.log(
            "[checkUploadPermission] Zone governance address:",
            zoneGovAddress
        );
        if (!zoneGovAddress)
            throw new Error("Zone governance address not found");

        const minioGovAddress = await fetchGovAddressFromDID(MINIO_DID);
        console.log(
            "[checkUploadPermission] MinIO governance address:",
            minioGovAddress
        );
        if (!minioGovAddress)
            throw new Error("MINIO governance address not found");

        const client = await CosmWasmClient.connect(AXONE_NODE_RPC);
        console.log("[checkUploadPermission] Connected to Axone node");

        // Step 1 - Check 'store' permission
        console.log("[checkUploadPermission] Checking store permission");
        const storeQuery = {
            ask: {
                query: `tell('${userDID}', 'store', Result, Evidence).`,
            },
        };
        const storeRes = await client.queryContractSmart(
            minioGovAddress,
            storeQuery
        );
        console.log(
            "[checkUploadPermission] Store permission response:",
            JSON.stringify(storeRes)
        );

        const storeAllowed = isPermittedResult(storeRes);
        console.log(
            "[checkUploadPermission] Store permission allowed:",
            storeAllowed
        );

        if (!storeAllowed) {
            console.warn(
                "[checkUploadPermission] 'store' permission denied by Prolog"
            );
            return false;
        }

        // Step 2 - Check MIME type permission
        console.log("[checkUploadPermission] Checking MIME type permission");
        const typeAction = `validate:file:${file.type}`;
        const typeQuery = {
            ask: {
                query: `tell('${userDID}', '${typeAction}', Result, Evidence).`,
            },
        };
        const typeRes = await client.queryContractSmart(
            minioGovAddress,
            typeQuery
        );
        console.log(
            "[checkUploadPermission] MIME type permission response:",
            JSON.stringify(typeRes)
        );

        const typeAllowed = isPermittedResult(typeRes);
        console.log(
            "[checkUploadPermission] MIME type permission allowed:",
            typeAllowed
        );

        if (!typeAllowed) {
            console.warn(
                `[checkUploadPermission] File type '${file.type}' not permitted by Prolog`
            );
            return false;
        }

        // Step 3 - Check file size permission
        console.log("[checkUploadPermission] Checking file size permission");
        const sizeAction = `validate:file:size:${file.size}`;
        const sizeQuery = {
            ask: {
                query: `tell('${userDID}', '${sizeAction}', Result, Evidence).`,
            },
        };
        const sizeRes = await client.queryContractSmart(
            minioGovAddress,
            sizeQuery
        );
        console.log(
            "[checkUploadPermission] File size permission response:",
            JSON.stringify(sizeRes)
        );

        const sizeAllowed = isPermittedResult(sizeRes);
        console.log(
            "[checkUploadPermission] File size permission allowed:",
            sizeAllowed
        );

        if (!sizeAllowed) {
            console.warn(
                `[checkUploadPermission] File size ${file.size} bytes not permitted by Prolog`
            );
            return false;
        }

        // Step 4 - Check if user is contributor
        console.log(
            "[checkUploadPermission] Checking if user is a contributor"
        );
        const isContributor = await queryIsContributor(zoneGovAddress, userDID);
        console.log(
            "[checkUploadPermission] User is contributor:",
            isContributor
        );

        // Step 5 - If not contributor, check txHash
        if (!isContributor) {
            console.log(
                "[checkUploadPermission] User is not a contributor, checking payment"
            );
            if (!txHash) {
                console.error(
                    "[checkUploadPermission] Missing txHash for guest user"
                );
                return false;
            }
            const paid = await checkPaymentMock(txHash);
            console.log("[checkUploadPermission] Payment check result:", paid);
            return paid;
        }

        console.log(
            "[checkUploadPermission] All checks passed, user has permission"
        );
        return true;
    } catch (error) {
        console.error("[checkUploadPermission] Unexpected error:", error);
        return false;
    }
}

/**
 * Helper function to check if a query result indicates permission
 * @param res The query result
 * @returns True if the result indicates permission, false otherwise
 */
export function isPermittedResult(res: any): boolean {
    const isPermitted =
        res?.answer?.results?.[0]?.substitutions?.find(
            (s: any) => s.variable === "Result"
        )?.expression === "permitted";

    console.log(`[isPermittedResult] Result: ${isPermitted}`);
    return isPermitted;
}

/**
 * Mock function to check payment validity
 * @param txHash Transaction hash to check
 * @returns True if payment is valid, false otherwise
 */
export async function checkPaymentMock(txHash: string): Promise<boolean> {
    console.log(`[checkPaymentMock] Checking payment for txHash: ${txHash}`);
    // Replace with tx proof check
    return true;
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

export async function getRawPrivkeyFromMnemonic(
    mnemonic: string
): Promise<Uint8Array> {
    const hdPath = stringToPath("m/44'/118'/0'/0/0");
    const seed = await Bip39.mnemonicToSeed(new EnglishMnemonic(mnemonic));
    const { privkey } = Slip10.derivePath(Slip10Curve.Secp256k1, seed, hdPath);
    return privkey;
}

/**
 * Main function to create and publish a credential for a dataset
 * @param file File uploaded by the user
 * @returns Transaction hash on the blockchain
 */
export async function createAndPublishCredential(file: File): Promise<string> {
    try {
        console.log(
            `[createAndPublishCredential] Starting for file: ${file.name}`
        );

        // Generate a unique DID for the dataset
        const datasetDID = await generateRandomDidKey();
        console.log(
            `[createAndPublishCredential] Generated dataset DID: ${datasetDID}`
        );

        // Get issuer information from the mnemonic phrase
        console.log(
            `[createAndPublishCredential] Creating wallet from mnemonic`
        );
        const wallet = await DirectSecp256k1HdWallet.fromMnemonic(
            MINIO_WALLET_MNEMONIC_PHRASE,
            { prefix: "axone" }
        );

        const [account] = await wallet.getAccounts();
        const issuerAddress = account.address;
        console.log(
            `[createAndPublishCredential] Issuer address: ${issuerAddress}`
        );

        // Derive the issuer DID from the address
        // IMPORTANT: Changed the DID format to be compatible with Axone
        const issuerDID = MINIO_DID;
        console.log(`[createAndPublishCredential] Issuer DID: ${issuerDID}`);
        const issuerName = process.env.MINIO_WALLET || "minio-wallet";

        // Extract file metadata
        const fileType = file.type || "application/octet-stream";
        const fileName = file.name || "Unknown File";
        console.log(
            `[createAndPublishCredential] File metadata: ${fileName}, ${fileType}, ${file.size} bytes`
        );

        // Generate the credential
        console.log(`[createAndPublishCredential] Generating credential`);
        const credential = generateDatasetCredential({
            datasetDID,
            issuerDID,
            issuerName,
            fileName,
            fileType,
            fileSize: file.size,
        });
        console.log(
            `[createAndPublishCredential] Credential generated: ${JSON.stringify(
                credential
            )}`
        );

        // Sign the credential
        console.log(`[createAndPublishCredential] Signing credential`);
        const signedCredential = await signCredential(credential, wallet);
        console.log(
            `[createAndPublishCredential] Credential signed, proof: ${JSON.stringify(
                signedCredential.proof
            )}`
        );

        // Sign the credential
        console.log(`[createAndPublishCredential] Signing credential2`);
        const signedCredential = await signCredential2(credential, wallet);
        console.log(
            `[createAndPublishCredential] Credential signed, proof: ${JSON.stringify(
                signedCredential.proof
            )}`
        );

        // Convert to N-Quads format
        console.log(`[createAndPublishCredential] Converting to N-Quads`);
        const nquads = await convertToNQuads(signedCredential);
        console.log(
            `[createAndPublishCredential] N-Quads generated: ${nquads}`
        );

        // const signedCredentialnQuads = await signCredentialViaCLI(
        //     credential,
        //     issuerName
        // );
        // console.log(
        //     `[createAndPublishCredential] N-Quads generated ALTERNATIVE: ${signedCredentialnQuads}`
        // );

        // Publish to the blockchain
        console.log(`[createAndPublishCredential] Publishing to blockchain`);
        const txHash = await publishCredentialToChain(nquads, wallet);
        console.log(
            `[createAndPublishCredential] Published successfully, txHash: ${txHash}`
        );

        return txHash;
    } catch (error) {
        console.error("[createAndPublishCredential] Error:", error);
        throw error;
    }
}

/**
 * Generates a dataset description credential
 * @param params Parameters for generating the credential
 * @returns JSON-LD credential object
 */
export function generateDatasetCredential({
    datasetDID,
    issuerDID,
    issuerName,
    fileName,
    fileType,
    fileSize,
}: {
    datasetDID: string;
    issuerDID: string;
    issuerName: string;
    fileName: string;
    fileType: string;
    fileSize: number;
}) {
    console.log(
        `[generateDatasetCredential] Generating credential for dataset: ${fileName}`
    );

    // Map file type to format URI
    let formatUri =
        "https://w3id.org/axone/ontology/v4/thesaurus/media-type/application_octet-stream";

    if (fileType.includes("csv")) {
        formatUri =
            "https://w3id.org/axone/ontology/v4/thesaurus/media-type/text_csv";
    } else if (fileType.includes("json")) {
        formatUri =
            "https://w3id.org/axone/ontology/v4/thesaurus/media-type/application_json";
    } else if (fileType.includes("pdf")) {
        formatUri =
            "https://w3id.org/axone/ontology/v4/thesaurus/media-type/application_pdf";
    }
    console.log(
        `[generateDatasetCredential] Selected format URI: ${formatUri}`
    );

    // Generate tags based on file name and type
    const tags = ["DoctAI", "Dataset"];

    if (
        fileName.toLowerCase().includes("cardiac") ||
        fileName.toLowerCase().includes("heart")
    ) {
        tags.push("Cardiac");
    }

    if (
        fileName.toLowerCase().includes("health") ||
        fileType.includes("dicom")
    ) {
        tags.push("Health");
    }
    console.log(
        `[generateDatasetCredential] Generated tags: ${tags.join(", ")}`
    );

    // Create and return the credential object
    const credential = {
        "@context": [
            "https://www.w3.org/2018/credentials/v1",
            "https://w3id.org/axone/ontology/v4/schema/credential/dataset/description/",
        ],
        type: ["VerifiableCredential", "DatasetDescriptionCredential"],
        id: `https://w3id.org/axone/ontology/v4/schema/credential/dataset/description/${crypto.randomUUID()}`,
        credentialSubject: {
            id: datasetDID,
            hasTitle: `${fileName} - DoctAI Dataset`,
            hasDescription: `Medical dataset uploaded to DoctAI platform. File: ${fileName}, Size: ${formatFileSize(
                fileSize
            )}`,
            hasFormat: formatUri,
            hasTag: tags,
            hasGeoCoverage:
                "https://w3id.org/axone/ontology/v4/thesaurus/area-code/840",
            hasTemporalCoverage: `https://w3id.org/axone/ontology/v4/thesaurus/temporal-range/${
                new Date().getFullYear() - 1
            }_01_01_${new Date().getFullYear() + 1}_12_31`,
            hasTopic:
                "https://w3id.org/axone/ontology/v4/thesaurus/topic/health",
        },
        issuanceDate: new Date().toISOString(),
        issuer: {
            id: issuerDID,
        },
    };

    console.log(
        `[generateDatasetCredential] Credential generated successfully`
    );
    return credential;
}

/**
 * Signs a credential using the axoned CLI + jsonld CLI
 * @param credential The JSON-LD credential to sign
 * @param issuer_wallet_name The name of the issuer wallet
 * @returns Signed credential in N-Quads format
 */
export async function signCredentialViaCLI(
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
        //await execAsync(cmd, { shell: "/bin/bash" });
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
        // await unlink(tmpJsonld).catch(() => {});
        // await unlink(tmpNquads).catch(() => {});
    }
}

/**
 * Signs a credential using the wallet and rdf-canonize for URDNA2015 normalization
 * @param credential Credential to sign
 * @param wallet Wallet with the private key
 * @returns Signed credential with proof
 */
export async function signCredential2(
    credential: any,
    wallet: DirectSecp256k1HdWallet
): Promise<any> {
    try {
        console.log(`[signCredential] Init`);

        const [account] = await wallet.getAccounts();
        console.log(`[signCredential] Using account: ${account.address}`);

        const documentToSign = { ...credential };
        delete documentToSign.proof;

        const privKey = await getRawPrivkeyFromMnemonic(
            MINIO_WALLET_MNEMONIC_PHRASE
        );


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

        // const key = ec.keyFromPrivate(privKey);
        // const signature = key.sign(messageHash);

        // // Get DER-encoded signature
        // const derSig = signature.toDER();
        // const signatureBase64 = Buffer.from(derSig).toString("base64url");

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
 * @param wallet Wallet with the private key
 * @returns Signed credential
 */
export async function signCredential(
    credential: any,
    wallet: DirectSecp256k1HdWallet
): Promise<any> {
    try {
        console.log(`[signCredential] Init`);

        // Extract account from wallet
        const [account] = await wallet.getAccounts();
        console.log(`[signCredential] Using account: ${account.address}`);

        // Clone the credential object and remove any existing proof
        const documentToSign = { ...credential };
        delete documentToSign.proof;
        console.log(
            `[signCredential] Prepared document for signing: ${JSON.stringify(
                documentToSign
            )}`
        );

        // Get the private key
        const privKey = await getRawPrivkeyFromMnemonic(
            MINIO_WALLET_MNEMONIC_PHRASE
        );

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
 * Converts a credential to N-Quads format
 * @param credential Credential to convert
 * @returns N-Quads representation of the credential
 */
async function convertToNQuads(credential: any): Promise<string> {
    try {
        console.log(
            `[convertToNQuads] Converting credential to N-Quads format`
        );

        const result = await jsonld.toRDF(credential, {
            format: "application/n-quads",
        });

        const resultStr =
            typeof result === "string" ? result : result.toString();
        console.log(
            `[convertToNQuads] Conversion successful, N-Quads length: ${resultStr.length}`
        );

        return resultStr;
    } catch (error) {
        console.error("[convertToNQuads] Error converting to N-Quads:", error);
        throw new Error("Failed to convert credential to N-Quads format");
    }
}

/**
 * Publishes a credential to the Axone blockchain
 * @param nquads Credential in N-Quads format
 * @param wallet Issuer's wallet
 * @returns Transaction hash
 */
async function publishCredentialToChain(
    nquads: string,
    wallet: DirectSecp256k1HdWallet
): Promise<string> {
    try {
        console.log(
            `[publishCredentialToChain] Publishing credential to blockchain`
        );

        // Get the first account from the wallet
        const [account] = await wallet.getAccounts();
        console.log(
            `[publishCredentialToChain] Using account: ${account.address}`
        );

        // Connect to the blockchain using the wallet
        console.log(
            `[publishCredentialToChain] Connecting to Axone node: ${AXONE_NODE_RPC}`
        );
        const client = await SigningCosmWasmClient.connectWithSigner(
            AXONE_NODE_RPC,
            wallet,
            {
                gasPrice: GasPrice.fromString("0.025uaxone"),
            }
        );

        console.log(`[publishCredentialToChain] Connected to blockchain`);

        // Prepare the message for the contract
        const claims = Buffer.from(nquads).toString("base64");
        console.log(`[publishCredentialToChain] Claims: ${nquads}`);
        console.log(
            `[publishCredentialToChain] Claims prepared (${claims.length} bytes)`
        );

        const msg = {
            submit_claims: {
                claims: claims,
                format: "n_quads",
            },
        };
        console.log(
            `[publishCredentialToChain] Submitting to Dataverse contract: ${DATAVERSE_ADDR}`
        );

        // Execute the transaction
        const result = await client.execute(
            account.address,
            DATAVERSE_ADDR,
            msg,
            "auto"
        );

        console.log(
            `[publishCredentialToChain] Transaction successful, hash: ${result.transactionHash}`
        );
        return result.transactionHash;
    } catch (error) {
        console.error(
            "[publishCredentialToChain] Error publishing credential:",
            error
        );
        throw new Error(
            `Failed to publish credential to blockchain: ${
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
function formatFileSize(bytes: number): string {
    if (bytes < 1024) return bytes + " bytes";
    else if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(2) + " KB";
    else if (bytes < 1024 * 1024 * 1024)
        return (bytes / (1024 * 1024)).toFixed(2) + " MB";
    else return (bytes / (1024 * 1024 * 1024)).toFixed(2) + " GB";
}
