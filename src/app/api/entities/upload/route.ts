// src/app/api/entities/upload/route.ts

export const dynamic = "force-dynamic"; // Required to allow POST requests with formData in App Router

import { Chat } from "@/entities/Chat";
import { PostgreSQLDatabaseService } from "@/lib/backend/PostgreSQLDatabaseService";
import { getUserFromRequest } from "@/lib/backend/utils";
import {
    MINIO_BUCKET,
    MINIO_ENDPOINT,
    MINIO_PASSWORD,
    MINIO_PORT,
    MINIO_REGION,
    MINIO_USE_SSL,
    MINIO_USER,
} from "@/lib/constants";
import {
    checkUploadPermission,
    createAndPublishCredential,
} from "@/lib/cosmos/backend/axone";
import { PutObjectCommand, S3Client } from "@aws-sdk/client-s3";
import { NextRequest, NextResponse } from "next/server";

function isFileAllowed(file: File) {
    const validTypes = ["text/csv", "application/json"];
    return validTypes.includes(file.type) && file.size <= 50 * 1024 * 1024;
}

const s3 = new S3Client({
    region: MINIO_REGION,
    endpoint: `${
        MINIO_USE_SSL ? "https" : "http"
    }://${MINIO_ENDPOINT}:${MINIO_PORT}`,
    credentials: {
        accessKeyId: MINIO_USER,
        secretAccessKey: MINIO_PASSWORD,
    },
    forcePathStyle: true,
});

export async function POST(req: NextRequest) {
    try {
        console.log("[Upload] Incoming request...");

        const data = await req.formData();
        const file = data.get("file") as File;
        const txHash = data.get("txHash") as string | null;

        console.log("[Upload] File info:", {
            name: file?.name,
            size: file?.size,
            type: file?.type,
            txHash,
        });

        let userId: string;
        let userDID: string;
        let userType: "contributor" | "guest";
        let userAddress: string;

        try {
            const user = await getUserFromRequest(req);
            userId = user.id;
            userDID = user.userDID;
            userType = user.userType;
            userAddress = user.address;

            console.log("[Upload] Authenticated user:", {
                userId,
                userDID,
                userType,
                userAddress,
            });
        } catch (authError) {
            console.warn("[Upload] Unauthorized request:", authError);
            return NextResponse.json(
                { error: "Unauthorized" },
                { status: 401 }
            );
        }

        if (!file || !isFileAllowed(file)) {
            console.warn("[Upload] Rejected due to invalid file:", {
                name: file?.name,
                size: file?.size,
                type: file?.type,
            });
            return NextResponse.json(
                { error: "Invalid file" },
                { status: 400 }
            );
        }

        console.log("[Upload] Checking upload permissions...");
        const canUpload = await checkUploadPermission(userDID, file, txHash);
        if (!canUpload) {
            console.warn("[Upload] Permission check failed for:", userDID);
            return NextResponse.json(
                {
                    error: "Upload not permitted",
                    reason: "User not allowed by MinIO governance rules (Prolog check failed)",
                    did: userDID,
                    filename: file.name,
                    userType,
                },
                { status: 403 }
            );
        }

        console.log("[Upload] Upload permitted. Creating credential...");
        const { datasetDID, txHash: datasetCredentialTxHash } = await createAndPublishCredential(file);
        console.log(`[Upload] Credential created with ID: ${datasetDID} and txHash: ${datasetCredentialTxHash}`); 

        const buffer = Buffer.from(await file.arrayBuffer());
        const fileKey = `${userDID}/${Date.now()}_${file.name}`;

        console.log("[Upload] Uploading file to MinIO:", fileKey);
        await s3.send(
            new PutObjectCommand({
                Bucket: MINIO_BUCKET,
                Key: fileKey,
                Body: buffer,
                ContentType: file.type,
                Metadata: {
                    filename: file.name,
                    filesize: file.size.toString(),
                    status: "uploaded",
                },
            })
        );
        console.log("[Upload] File uploaded to MinIO.");

        const datasetUrl = `${
            MINIO_USE_SSL ? "https" : "http"
        }://${MINIO_ENDPOINT}:${MINIO_PORT}/${MINIO_BUCKET}/${fileKey}`;

        const db = await PostgreSQLDatabaseService.getDataSource();
        const chat = db.getRepository(Chat).create({
            title: `Chat on ${file.name}`,
            userId,
            datasetUrl,
            datasetDID,
            datasetCredentialTxHash,
        });
        await db.getRepository(Chat).save(chat);

        console.log("[Upload] Chat created with ID:", chat.id);
        return NextResponse.json(chat);
    } catch (error: any) {
        const reason =
            error instanceof Error
                ? error.message
                : typeof error === "object"
                ? JSON.stringify(error)
                : String(error);

        console.error("[Upload Error]", error);
        return NextResponse.json(
            { error: "Internal Server Error", reason },
            { status: 500 }
        );
    }
}
