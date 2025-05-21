// src/app/api/entities/upload/confirm-tx/route.ts

import { Chat } from "@/entities/Chat";
import { uploadToMinIO } from "@/lib/backend/minio";
import { PostgreSQLDatabaseService } from "@/lib/backend/PostgreSQLDatabaseService";
import { getUserFromRequest } from "@/lib/backend/utils";
import { AXONE_NODE_RPC } from "@/lib/constants";
import { extractDatasetDIDFromTx, validateDatasetDIDForConfirmation } from "@/lib/cosmos/backend/utils-tx";
import { waitForTxConfirmation } from "@/lib/cosmos/frontend/keplr";
import { StargateClient } from "@cosmjs/stargate";
import { NextRequest, NextResponse } from "next/server";

export async function POST(req: NextRequest) {
  try {
    console.log("[ConfirmTx] Starting confirmation process");

    const user = await getUserFromRequest(req);
    const { id: userId, userDID } = user;

    const form = await req.formData();
    const txHash = form.get("txHash")?.toString();
    const file = form.get("file") as File;

    if (!txHash || !file) {
      console.warn("[ConfirmTx] Missing txHash or file in request");
      return NextResponse.json(
        { error: "Missing required fields" },
        { status: 400 }
      );
    }

    console.log("[ConfirmTx] Waiting for tx to be confirmed:", txHash);
    await waitForTxConfirmation(txHash);

    const client = await StargateClient.connect(AXONE_NODE_RPC);
    const tx = await client.getTx(txHash);

    if (!tx) {
      console.error("[ConfirmTx] Transaction not found");
      return NextResponse.json(
        { error: "Transaction not found on chain" },
        { status: 404 }
      );
    }

    if (tx.code !== 0) {
      console.error("[ConfirmTx] Transaction failed:", tx.rawLog);
      return NextResponse.json(
        { error: "Transaction failed", details: tx.rawLog },
        { status: 400 }
      );
    }

    // Extract datasetDID from tx
    const datasetDID = extractDatasetDIDFromTx(tx.tx as Uint8Array);
    if (!datasetDID) {
      return NextResponse.json(
        { error: "Failed to extract datasetDID from transaction" },
        { status: 400 }
      );
    }

    console.log("[ConfirmTx] Extracted datasetDID:", datasetDID);

    // Validate that the chat exists and belongs to the user
    const chat = await validateDatasetDIDForConfirmation(datasetDID, userId);
    if (!chat) {
      console.error("[ConfirmTx] No valid chat found for confirmation");
      return NextResponse.json(
        { error: "Chat not found or not in valid state" },
        { status: 404 }
      );
    }

    // Upload file to MinIO
    console.log("[ConfirmTx] Uploading dataset to MinIO...");
    const datasetUrl = await uploadToMinIO(userDID, file);
    console.log("[ConfirmTx] Upload successful:", datasetUrl);

    // Activate chat
    chat.status = "active";
    chat.datasetCredentialTxHash = txHash;
    chat.datasetUrl = datasetUrl;
    await PostgreSQLDatabaseService.getDataSource().then((db) =>
      db.getRepository(Chat).save(chat)
    );

    console.log("[ConfirmTx] ✅ Chat activated:", chat.id);
    return NextResponse.json(chat);
  } catch (error) {
    console.error("[ConfirmTx] Fatal error:", error);
    return NextResponse.json(
      { error: "Confirmation failed", details: String(error) },
      { status: 500 }
    );
  }
}
