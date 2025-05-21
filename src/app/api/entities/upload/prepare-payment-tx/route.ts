// src/app/api/entities/upload/prepare-payment-tx/route.ts
import { NextRequest, NextResponse } from "next/server";
import { getUserFromRequest } from "@/lib/backend/utils";
import { buildGuestPaymentPayload } from "@/lib/cosmos/backend/utils-tx";
import { PostgreSQLDatabaseService } from "@/lib/backend/PostgreSQLDatabaseService";
import { Chat } from "@/entities/Chat";
import { checkUploadPermission } from "@/lib/cosmos/backend/axone";

export async function POST(req: NextRequest) {
    try {
        console.log("[PreparePaymentTx] Starting guest payment setup...");

        const user = await getUserFromRequest(req);
        const { address: userAddress, userType, id: userId } = user;

        if (userType !== "guest") {
            console.warn("[PreparePaymentTx] Only guests must pay.");
            return NextResponse.json(
                { error: "Only guests must pay" },
                { status: 403 }
            );
        }

        const form = await req.formData();
        const file = form.get("file") as File;
        if (!file) {
            console.warn("[PreparePaymentTx] Missing file in request");
            return NextResponse.json(
                { error: "Missing file" },
                { status: 400 }
            );
        }

        if (!(await checkUploadPermission(user.userDID, file))) {
            return NextResponse.json(
                { error: "Upload not permitted" },
                { status: 403 }
            );
        }

        const { datasetDID, payment } = await buildGuestPaymentPayload(
            userAddress
        );
        const title = `Chat on dataset ${datasetDID.slice(-6)}`;

        const db = await PostgreSQLDatabaseService.getDataSource();
        const chat = db.getRepository(Chat).create({
            title,
            userId,
            datasetDID,
            status: "waiting_payment",
        });

        await db.getRepository(Chat).save(chat);

        console.log(
            "[PreparePaymentTx] Chat created with status 'waiting_payment':",
            chat.id
        );

        return NextResponse.json({
            datasetDID,
            payment,
            chatId: chat.id,
        });
    } catch (error) {
        console.error(
            "[PreparePaymentTx] Failed to prepare guest payment:",
            error
        );
        return NextResponse.json(
            { error: "Failed to prepare guest payment" },
            { status: 500 }
        );
    }
}
