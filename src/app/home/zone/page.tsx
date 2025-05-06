"use client";

// app/(zone)/zone/page.tsx
import { useAxoneStore } from "@/store/axoneStore";
import { Landmark } from "lucide-react";
import styles from "./page.module.scss";

export default function ZonePage() {
    const {
        zoneDID,
        minioDID,
        zoneGovAddress,
        minioGovAddress,
        zoneGovCode,
        minioGovCode,
    } = useAxoneStore();

    return (
        <div className={styles.zoneContainer}>
            {/* Zone Card */}
            <div className={styles.card}>
                <div className={styles.icon}>
                    <Landmark />
                </div>
                <div className={styles.details}>
                    <h2 className={styles.title}>Zone: Hearth Labs</h2>
                    <p><strong>DID:</strong> {zoneDID}</p>
                    <p><strong>Governance Contract:</strong> {zoneGovAddress}</p>
                </div>
            </div>

            {/* Minio Card */}
            <div className={styles.card}>
                <div className={styles.icon}>
                    <Landmark />
                </div>
                <div className={styles.details}>
                    <h2 className={styles.title}>MinIO</h2>
                    <p><strong>DID:</strong> {minioDID}</p>
                    <p><strong>Governance Contract:</strong> {minioGovAddress}</p>
                </div>
            </div>

            {/* Zone Governance Code */}
            <div className={styles.codeSection}>
                <h3>Zone Governance Code</h3>
                <pre className={styles.codeBlock}>
                    {zoneGovCode}
                </pre>
            </div>

            {/* MinIO Governance Code */}
            <div className={styles.codeSection}>
                <h3>MinIO Governance Code</h3>
                <pre className={styles.codeBlock}>
                    {minioGovCode}
                </pre>
            </div>
        </div>
    );
}
