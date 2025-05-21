// src/app/api/entities/upload/prepare-upload-tx/route.ts

import { Chat } from "@/entities/Chat";
import { PostgreSQLDatabaseService } from "@/lib/backend/PostgreSQLDatabaseService";
import { getUserFromRequest } from "@/lib/backend/utils";
import { checkUploadPermission } from "@/lib/cosmos/backend/axone";
import { generateRandomDidKey } from "@/lib/cosmos/backend/utils";
import {
    buildCredentialTxFromFile,
    validateGuestPaymentTx,
} from "@/lib/cosmos/backend/utils-tx";
import { MsgExecuteContract } from "cosmjs-types/cosmwasm/wasm/v1/tx";
import { NextRequest, NextResponse } from "next/server";

export async function POST(req: NextRequest) {
    try {
        console.log("[PrepareUploadTx] Preparing credential transaction");

        const user = await getUserFromRequest(req);
        const { userDID, address: userAddress, id: userId, userType } = user;

        const form = await req.formData();
        const file = form.get("file") as File;
        if (!file) {
            console.warn("[PrepareUploadTx] Missing file in request");
            return NextResponse.json(
                { error: "Missing file" },
                { status: 400 }
            );
        }

        const db = await PostgreSQLDatabaseService.getDataSource();

        let chat: Chat | null = null;
        let datasetDID = form.get("datasetDID")?.toString() || null;
        const paymentTxHash = form.get("paymentTxHash")?.toString() || null;

        // === Guest flow ===
        if (userType === "guest") {
            if (!datasetDID || !paymentTxHash) {
                console.warn(
                    "[PrepareUploadTx] Missing datasetDID or paymentTxHash"
                );
                return NextResponse.json(
                    { error: "Missing datasetDID or paymentTxHash" },
                    { status: 400 }
                );
            }

            chat = await db
                .getRepository(Chat)
                .findOneBy({ datasetDID, userId });
            if (!chat || chat.status !== "waiting_payment") {
                console.error(
                    "[PrepareUploadTx] Chat not found or invalid status:",
                    {
                        datasetDID,
                        userId,
                        status: chat?.status,
                    }
                );
                return NextResponse.json(
                    { error: "Invalid or missing Chat for datasetDID" },
                    { status: 400 }
                );
            }

            const isValid = await validateGuestPaymentTx(
                datasetDID,
                paymentTxHash,
                userAddress
            );
            if (!isValid) {
                console.error("[PrepareUploadTx] Payment validation failed");
                return NextResponse.json(
                    { error: "Invalid payment transaction" },
                    { status: 400 }
                );
            }

            chat.paymentTxHash = paymentTxHash;
            chat.status = "paid_pending_upload";
            await db.getRepository(Chat).save(chat);
            console.log(
                "[PrepareUploadTx] Chat validated and updated:",
                chat.id
            );
        }

        // === Contributor flow ===
        if (userType !== "guest") {
            const title =
                form.get("title")?.toString() || `Chat on ${file.name}`;
            datasetDID = await generateRandomDidKey();

            const newChat = db.getRepository(Chat).create({
                title,
                userId,
                datasetDID,
                status: "pending",
                createdAt: new Date(),
            });
            await db.getRepository(Chat).save(newChat);
            chat = newChat;
            console.log("[PrepareUploadTx] New chat created:", chat.id);
        }

        // === Build credential tx ===
        if (!chat) {
            console.error("[PrepareUploadTx] Internal error: chat not set");
            return NextResponse.json(
                { error: "Chat not initialized" },
                { status: 500 }
            );
        }

        if (!(await checkUploadPermission(user.userDID, file))) {
            return NextResponse.json(
                { error: "Upload not permitted" },
                { status: 403 }
            );
        }

        const { message } = await buildCredentialTxFromFile(
            file,
            userAddress,
            datasetDID
        );
        const decodedMsg = MsgExecuteContract.decode(message);
        const executeMsgJson = JSON.parse(
            Buffer.from(decodedMsg.msg).toString("utf-8")
        );

        return NextResponse.json({
            datasetDID,
            chatId: chat.id,
            fileName: file.name,
            contractAddress: decodedMsg.contract,
            executeMsg: executeMsgJson,
        });
    } catch (error) {
        console.error("[PrepareUploadTx] Unexpected error:", error);
        return NextResponse.json(
            { error: "Failed to prepare upload tx" },
            { status: 500 }
        );
    }
}
