// src/lib/backend/minio.ts

import {
    MINIO_BUCKET,
    MINIO_ENDPOINT,
    MINIO_PASSWORD,
    MINIO_PORT,
    MINIO_REGION,
    MINIO_USE_SSL,
    MINIO_USER,
} from "@/lib/constants";
import { PutObjectCommand, S3Client } from "@aws-sdk/client-s3";

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

export async function uploadToMinIO(
    userDID: string,
    file: File
): Promise<string> {
    const buffer = Buffer.from(await file.arrayBuffer());
    const fileKey = `${userDID}/${Date.now()}_${file.name}`;

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

    return `${
        MINIO_USE_SSL ? "https" : "http"
    }://${MINIO_ENDPOINT}:${MINIO_PORT}/${MINIO_BUCKET}/${fileKey}`;
}
