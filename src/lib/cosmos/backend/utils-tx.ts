// src/lib/cosmos/backend/utils-tx.ts

import { Chat } from "@/entities/Chat";
import { PostgreSQLDatabaseService } from "@/lib/backend/PostgreSQLDatabaseService";
import {
    AXONE_DENOM,
    AXONE_NODE_RPC,
    DATAVERSE_ADDR,
    MINIO_DID,
    MINIO_WALLET_MNEMONIC_PHRASE,
} from "@/lib/constants";
import { generateDatasetCredential } from "@/lib/cosmos/backend/axone";
import {
    generateRandomDidKey,
    signCredentialWithJwk,
} from "@/lib/cosmos/backend/utils";
import { convertToNQuads } from "@/lib/cosmos/backend/utils-jsonld-nquads";
import { toUtf8 } from "@cosmjs/encoding";
import { StargateClient } from "@cosmjs/stargate";
import { TxBody, TxRaw } from "cosmjs-types/cosmos/tx/v1beta1/tx";
import { MsgExecuteContract } from "cosmjs-types/cosmwasm/wasm/v1/tx";
import { In } from "typeorm";
import {
    fetchGovAddressFromDID,
    fetchZoneDIDFromMinIODID,
} from "../frontend/axone";
import { waitForTxConfirmation } from "../frontend/keplr";
import { MsgSend } from "cosmjs-types/cosmos/bank/v1beta1/tx";

/**
 * Builds a MsgSend payload for guest payment, linked to a new datasetDID.
 * @param userAddress - Wallet address of the user paying
 */
export async function buildGuestPaymentPayload(userAddress: string): Promise<{
    datasetDID: string;
    payment: {
        type: string;
        fromAddress: string;
        toAddress: string;
        amount: { denom: string; amount: string }[];
        memo: string;
    };
}> {
    console.log("[buildGuestPaymentPayload] Generating datasetDID...");
    const datasetDID = await generateRandomDidKey();
    console.log("[buildGuestPaymentPayload] datasetDID:", datasetDID);

    const zoneDID = await fetchZoneDIDFromMinIODID(MINIO_DID);
    if (!zoneDID) throw new Error("ZoneDID not found from MINIO_DID");

    const zoneGovAddress = await fetchGovAddressFromDID(zoneDID);
    if (!zoneGovAddress) throw new Error("Zone governance address not found");

    console.log("[buildGuestPaymentPayload] zoneGovAddress:", zoneGovAddress);

    return {
        datasetDID,
        payment: {
            type: "cosmos.bank.v1beta1.MsgSend",
            fromAddress: userAddress,
            toAddress: zoneGovAddress,
            amount: [
                {
                    denom: AXONE_DENOM,
                    amount: "1",
                },
            ],
            memo: datasetDID,
        },
    };
}

/**
 * Validates that the payment TxHash corresponds to a MsgSend to the zoneGovAddress,
 * sent by the user, and with memo === datasetDID
 */
export async function validateGuestPaymentTx(
    datasetDID: string,
    txHash: string,
    sender: string
): Promise<boolean> {
    try {
        console.log("[validateGuestPaymentTx] Validating tx:", txHash);

        console.log("[ConfirmTx] Waiting for tx to be confirmed:", txHash);
        await waitForTxConfirmation(txHash);

        const client = await StargateClient.connect(AXONE_NODE_RPC);
        const tx = await client.getTx(txHash);

        if (!tx) {
            console.error("[validateGuestPaymentTx] Transaction not found");
            return false;
        }

        const txRaw = TxRaw.decode(tx.tx as Uint8Array);
        const txBody = TxBody.decode(txRaw.bodyBytes);

        const msgAny = txBody.messages[0];

        if (msgAny.typeUrl !== "/cosmos.bank.v1beta1.MsgSend") {
            console.warn(
                "[validateGuestPaymentTx] Unexpected message type:",
                msgAny.typeUrl
            );
            return false;
        }

        const memo = txBody.memo;

        const decodedMsg = MsgSend.decode(msgAny.value);

        if (decodedMsg.fromAddress !== sender || memo !== datasetDID) {
            console.warn("[validateGuestPaymentTx] MsgSend field mismatch", {
                from: decodedMsg.fromAddress,
                expectedFrom: sender,
                memo,
                expectedMemo: datasetDID,
            });
            return false;
        }

        console.log("[validateGuestPaymentTx] ✅ Valid payment confirmed");
        return true;
    } catch (err) {
        console.error("[validateGuestPaymentTx] Unexpected error:", err);
        return false;
    }
}

/**
 * Builds a transaction for publishing a VC credential to the Dataverse contract
 */
export async function buildCredentialTxFromFile(
    file: File,
    userAddress: string,
    forcedDatasetDID: string | null
): Promise<{
    datasetDID: string;
    message: Uint8Array;
}> {
    console.log(
        `[buildCredentialTxFromFile] Starting build for file: ${file.name}`
    );

    // Step 1 - Generate dataset DID and VC
    const datasetDID = forcedDatasetDID || (await generateRandomDidKey());
    const credential = generateDatasetCredential({
        datasetDID,
        issuerDID: MINIO_DID,
        issuerName: "minio-wallet",
        fileName: file.name,
        fileType: file.type,
        fileSize: file.size,
    });

    console.log(`[buildCredentialTxFromFile] Credential generated`);

    // Step 2 - Sign VC and convert to N-Quads
    const signedVC = await signCredentialWithJwk(
        credential,
        MINIO_WALLET_MNEMONIC_PHRASE
    );
    const nquads = await convertToNQuads(signedVC);
    const base64Metadata = Buffer.from(nquads).toString("base64");

    console.log(`[buildCredentialTxFromFile] Credential signed and converted`);

    // Step 3 - Prepare MsgExecuteContract
    console.log("MsgExecuteContract msg:", {
        sender: userAddress,
        contract: DATAVERSE_ADDR,
        msg: {
            submit_claims: {
                claims: base64Metadata,
                format: "n_quads",
            },
        },
        funds: [],
    });

    const msg: MsgExecuteContract = {
        sender: userAddress,
        contract: DATAVERSE_ADDR,
        msg: toUtf8(
            JSON.stringify({
                submit_claims: {
                    claims: base64Metadata,
                    format: "n_quads",
                },
            })
        ),
        funds: [],
    };

    console.log("MsgExecuteContract encoded:", msg);

    return {
        datasetDID,
        message: MsgExecuteContract.encode(msg).finish(),
    };
}

/**
 * Extracts datasetDID from a MsgExecuteContract submit_claims transaction.
 */
export function extractDatasetDIDFromTx(txBytes: Uint8Array): string | null {
    try {
        const txRaw = TxRaw.decode(txBytes);
        const txBody = TxBody.decode(txRaw.bodyBytes);
        const msgAny = txBody.messages[0];
        const msgExecute = MsgExecuteContract.decode(msgAny.value);
        const msgStr = new TextDecoder().decode(msgExecute.msg);
        const contractMsg = JSON.parse(msgStr);
        const claimsBase64 = contractMsg.submit_claims?.claims;

        if (!claimsBase64) {
            console.error(
                "[extractDatasetDIDFromTx] Missing claims in submit_claims"
            );
            return null;
        }

        const claimsStr = Buffer.from(claimsBase64, "base64").toString("utf-8");
        const didKeyPattern = /did:key:z[1-9A-HJ-NP-Za-km-z]+/g;
        const matches = claimsStr.match(didKeyPattern);

        if (!matches || matches.length === 0) {
            console.error(
                "[extractDatasetDIDFromTx] No DID match found in claims"
            );
            return null;
        }

        return matches[0];
    } catch (err) {
        console.error("[extractDatasetDIDFromTx] Failed to parse tx:", err);
        return null;
    }
}

/**
 * Validates that the datasetDID belongs to the user and is in a pre-activation state.
 * Returns the Chat if valid, or null if invalid/missing.
 */
export async function validateDatasetDIDForConfirmation(
    datasetDID: string,
    userId: string
): Promise<Chat | null> {
    try {
        const db = await PostgreSQLDatabaseService.getDataSource();
        const chat = await db.getRepository(Chat).findOne({
            where: {
                datasetDID,
                userId,
                status: In(["pending", "paid_pending_upload"]),
            },
        });

        if (!chat) {
            console.warn(
                "[validateDatasetDIDForConfirmation] Chat not found or invalid state",
                {
                    datasetDID,
                    userId,
                }
            );
            return null;
        }

        return chat;
    } catch (err) {
        console.error(
            "[validateDatasetDIDForConfirmation] Unexpected error:",
            err
        );
        return null;
    }
}
