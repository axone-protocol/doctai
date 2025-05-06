"use client";
import { ROUTES } from "@/lib/routes";
import { useUserStore } from "@/store/useUserStore";
import { useRouter } from "next/navigation";
import { useState } from "react";
import { useDropzone } from "react-dropzone";
import styles from "./page.module.scss";
import { createTx } from "@/lib/cosmos/frontend/keplr";
import axios from "axios";

export default function ChatPage() {
    const { user } = useUserStore();
    const router = useRouter();
    const [file, setFile] = useState<File | null>(null);
    const [loading, setLoading] = useState(false);
    const [uploadProgress, setUploadProgress] = useState<number | null>(null);

    const onDrop = (acceptedFiles: File[]) => {
        if (acceptedFiles.length) {
            setFile(acceptedFiles[0]);
        }
    };

    // Dropzone config
    const { getRootProps, getInputProps, isDragActive } = useDropzone({
        onDrop,
        maxSize: 50 * 1024 * 1024, // 50MB
        accept: {
            "text/csv": [".csv"],
            "application/json": [".json"],
        },
    });

    const handleUpload = async () => {
        if (!file || !user?.id) return;
        setLoading(true);
        setUploadProgress(0);

        let txHash = null;
        if (user.userType !== "contributor") {
            txHash = await createTx(user.address, "axoneAddressHere", 1);
            if (!txHash) {
                alert("Payment failed");
                setLoading(false);
                return;
            }
        }

        const formData = new FormData();
        formData.append("file", file);
        formData.append("userId", user.id);
        if (txHash) formData.append("txHash", txHash);

        try {
            const res = await axios.post("/api/entities/upload", formData, {
                headers: {
                    "Content-Type": "multipart/form-data",
                },
                onUploadProgress: (event) => {
                    const percent = Math.round(
                        (event.loaded * 100) / (event.total || 1)
                    );
                    setUploadProgress(percent);
                },
            });

            const chat = res.data;
            router.push(`${ROUTES.CHAT_INTERACTION}/${chat.id}`);
        } catch (error: any) {
            const reason =
                error?.response?.data?.reason ||
                error?.response?.data?.error ||
                "Upload failed";

            console.error("Upload error:", error.response?.data || error);
            alert(reason);
        } finally {
            setLoading(false);
            setUploadProgress(null);
        }
    };

    return (
        <div className={styles.chatUploadContainer}>
            <h1>Upload Dataset to Start Chat with DoctAI</h1>
            <div className={styles.uploadBox} {...getRootProps()}>
                <input {...getInputProps()} />
                {isDragActive ? (
                    <p>Drop your CSV file here...</p>
                ) : (
                    <p>Drag and drop a CSV here, or click to select one.</p>
                )}
            </div>
            {file && (
                <div className={styles.fileInfo}>
                    <p>
                        <strong>Filename:</strong> {file.name}
                    </p>
                    <p>
                        <strong>Size:</strong>{" "}
                        {(file.size / 1024 / 1024).toFixed(2)} MB
                    </p>
                    {uploadProgress !== null && (
                        <progress value={uploadProgress} max={100}></progress>
                    )}
                </div>
            )}
            <div className={styles.uploadButtonWrapper}>
                <button
                    className={styles.uploadButton}
                    disabled={loading || !file}
                    onClick={handleUpload}
                >
                    {loading ? "Uploading..." : "Upload and Start Chat"}
                </button>
            </div>
        </div>
    );
}
