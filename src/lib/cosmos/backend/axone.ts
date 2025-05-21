// src/lib/cosmos/backend/axone.ts

import {
    AXONE_NODE_RPC,
    MINIO_DID
} from "@/lib/constants";
import {
    CosmWasmClient
} from "@cosmjs/cosmwasm-stargate";
import {
    fetchGovAddressFromDID,
    fetchZoneDIDFromMinIODID,
} from "../frontend/axone";
import {
    formatFileSize
} from "./utils";

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

        // TODO: mock response for testing
        // const isPermitted = true;
        // const isPermitted = false;
        return isPermitted ;
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
            // name: issuerName,
        },
    };

    console.log(
        `[generateDatasetCredential] Credential generated successfully`
    );
    return credential;
}
