// src/app/home/chat/page.tsx

"use client";
import {
    AXON_GAS_PRICE,
    AXONE_DENOM,
    AXONE_NODE_RPC,
    CHAIN_ID,
    DEFAULT_GAS_LIMIT,
} from "@/lib/constants";
import { getKeplr } from "@/lib/cosmos/frontend/keplr";
import { ROUTES } from "@/lib/routes";
import { useUserStore } from "@/store/useUserStore";
import { StdFee } from "@cosmjs/amino";
import { SigningCosmWasmClient } from "@cosmjs/cosmwasm-stargate";
import { toUtf8 } from "@cosmjs/encoding";
import { SigningStargateClient } from "@cosmjs/stargate";
import axios from "axios";
import { useRouter } from "next/navigation";
import { useState } from "react";
import { useDropzone } from "react-dropzone";
import styles from "./page.module.scss";

export default function ChatPage() {
    const { user } = useUserStore();
    const router = useRouter();
    const [file, setFile] = useState<File | null>(null);
    const [loading, setLoading] = useState(false);
    const [uploadProgress, setUploadProgress] = useState<number | null>(null);

    const onDrop = (acceptedFiles: File[]) => {
        if (acceptedFiles.length) setFile(acceptedFiles[0]);
    };

    const { getRootProps, getInputProps, isDragActive } = useDropzone({
        onDrop,
        maxSize: 50 * 1024 * 1024,
        accept: { "text/csv": [".csv"], "application/json": [".json"] },
    });

    const handleUpload = async () => {
        if (!file || !user?.address || !user?.userDID) {
            console.warn("[handleUpload] Missing file or user information");
            return;
        }

        console.log("[handleUpload] Starting upload process...");
        setLoading(true);
        setUploadProgress(0);

        try {
            const keplr = getKeplr();
            const signer = await keplr.getOfflineSigner(CHAIN_ID, {
                preferNoSetFee: true,
            });
            const [{ address }] = await signer.getAccounts();

            let datasetDID: string | null = null;
            let paymentTxHash: string | null = null;

            // STEP 1 - Guest payment
            if (user.userType === "guest") {
                console.log("[handleUpload] Preparing guest payment...");
                const form = new FormData();
                form.append("file", file);
                const { data } = await axios.post(
                    "/api/entities/upload/prepare-payment-tx",
                    form
                );
                datasetDID = data.datasetDID;
                const { fromAddress, toAddress, amount, memo } = data.payment;

                const fee: StdFee = {
                    amount: [{ denom: AXONE_DENOM, amount: "5000" }],
                    gas: DEFAULT_GAS_LIMIT.toString(),
                };

                const bankMsg = {
                    typeUrl: "/cosmos.bank.v1beta1.MsgSend",
                    value: {
                        fromAddress,
                        toAddress,
                        amount,
                    },
                };

                const stargateClient =
                    await SigningStargateClient.connectWithSigner(
                        AXONE_NODE_RPC,
                        signer
                    );
                const result = await stargateClient.signAndBroadcast(
                    address,
                    [bankMsg],
                    fee,
                    memo
                );

                if (result.code !== 0) {
                    console.error(
                        "[handleUpload] Payment tx failed:",
                        result.rawLog
                    );
                    throw new Error("Payment transaction failed");
                }

                paymentTxHash = result.transactionHash;
                console.log(
                    "[handleUpload] ✅ Guest payment success:",
                    paymentTxHash
                );
            }

            // STEP 2 - Prepare credential tx
            console.log("[handleUpload] Preparing upload tx...");
            const prepareForm = new FormData();
            prepareForm.append("file", file);
            if (datasetDID) prepareForm.append("datasetDID", datasetDID);
            if (paymentTxHash)
                prepareForm.append("paymentTxHash", paymentTxHash);

            const { data: uploadTx } = await axios.post(
                "/api/entities/upload/prepare-upload-tx",
                prepareForm
            );
            const { contractAddress, executeMsg } = uploadTx;

            // STEP 3 - Sign and broadcast MsgExecuteContract
            const wasmClient = await SigningCosmWasmClient.connectWithSigner(
                AXONE_NODE_RPC,
                signer
            );
            const fee: StdFee = {
                amount: [
                    {
                        denom: AXONE_DENOM,
                        amount: Math.ceil(
                            AXON_GAS_PRICE * DEFAULT_GAS_LIMIT
                        ).toString(),
                    },
                ],
                gas: DEFAULT_GAS_LIMIT.toString(),
            };

            const msgExecute = {
                typeUrl: "/cosmwasm.wasm.v1.MsgExecuteContract",
                value: {
                    sender: address,
                    contract: contractAddress,
                    msg: toUtf8(JSON.stringify(executeMsg)),
                    funds: [],
                },
            };

            const result = await wasmClient.signAndBroadcast(
                address,
                [msgExecute],
                fee,
                `Upload credential for ${file.name}`
            );

            if (result.code !== 0) {
                console.error(
                    "[handleUpload] Credential tx failed:",
                    result.rawLog
                );
                throw new Error("Credential transaction failed");
            }

            console.log(
                "[handleUpload] ✅ Credential tx broadcasted:",
                result.transactionHash
            );

            // STEP 4 - Confirm
            const confirmForm = new FormData();
            confirmForm.append("file", file);
            confirmForm.append("txHash", result.transactionHash);

            const { data: confirmedChat } = await axios.post(
                "/api/entities/upload/confirm-tx",
                confirmForm
            );

            console.log("[handleUpload] ✅ Chat confirmed:", confirmedChat.id);
            router.push(`${ROUTES.CHAT_INTERACTION}/${confirmedChat.id}`);
        } catch (error: any) {
            const reason =
                error?.response?.data?.reason ||
                error?.response?.data?.error ||
                error?.message ||
                "Upload failed";
            console.error("[handleUpload] Upload error:", reason);
            alert(reason);
        } finally {
            setLoading(false);
            setUploadProgress(null);
            console.log("[handleUpload] Upload process complete");
        }
    };

    return (
        <div className={styles.chatUploadContainer}>
            <h1>Upload Dataset to Start Chat with DoctAI</h1>
            <div className={styles.uploadBox} {...getRootProps()}>
                <input {...getInputProps()} />
                {isDragActive ? (
                    <p>Drop your file here...</p>
                ) : (
                    <p>Drag and drop a CSV or JSON file, or click to select.</p>
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
