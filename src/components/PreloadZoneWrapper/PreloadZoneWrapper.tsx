"use client";

import { useEffect, useState } from "react";
import { useAxoneStore, setAxoneStoreFromZoneInfo } from "@/store/axoneStore";
import { preloadAxoneInfoFromMinioDid } from "@/lib/frontend/bootstrap";
import { MINIO_DID } from "@/lib/constants";
import { Loader } from "lucide-react";
import styles from "./PreloadZoneWrapper.module.scss";
import Image from "next/image";

export default function PreloadZoneWrapper({
    children,
}: {
    children: React.ReactNode;
}) {
    const { zoneDID } = useAxoneStore();
    const [loading, setLoading] = useState(false);

    useEffect(() => {
        if (!zoneDID) {
            setLoading(true);
            preloadAxoneInfoFromMinioDid(MINIO_DID)
                .then(setAxoneStoreFromZoneInfo)
                .catch(console.error)
                .finally(() => setLoading(false));
        }
    }, [zoneDID]);

    if (!zoneDID || loading) {
        return (
            <div className={styles.loginWrapper}>
                <div className={styles.loginBox}>
                    <Image
                        src="/logo.png"
                        alt="DoctAI Logo"
                        width={250}
                        height={250}
                        priority
                    />
                    <p className={styles.subtitle}>
                        DoctAI is an AI-powered analysis platform for cardiac
                        datasets. Connect your wallet to get started.
                    </p>

                    <div className={styles.loading}>
                        <div className={styles.topRow}>
                            <span className={styles.icon}>
                                <Loader />
                            </span>
                            <span className={styles.dots}>
                                <span>.</span>
                                <span>.</span>
                                <span>.</span>
                            </span>
                        </div>
                        <p className={styles.loadingText}>
                            Loading Hearth Labs Data...
                        </p>
                    </div>
                </div>
            </div>
        );
    }

    return <>{children}</>;
}
