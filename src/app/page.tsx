"use client";

import { useEffect, useState } from "react";
import { useRouter } from "next/navigation";
import styles from "./page.module.scss";
import { setAxoneStoreFromZoneInfo } from "@/store/axoneStore";
import { MINIO_DID } from "@/lib/constants";
import { preloadAxoneInfoFromMinioDid } from "@/lib/frontend/bootstrap";

export default function PreloadPage() {
    const router = useRouter();
    const [loading, setLoading] = useState(true);

    useEffect(() => {
        const load = async () => {
            try {
                const zoneInfo = await preloadAxoneInfoFromMinioDid(MINIO_DID);
                setAxoneStoreFromZoneInfo(zoneInfo);
            } catch (err) {
                console.error("Error loading Axone zone info", err);
            } finally {
                setLoading(false);
                router.push("/login");
            }
        };
        load();
    }, []);

    return (
        <div className={styles.loaderContainer}>
            <h1>Loading Hearth Labs data...</h1>
            <p>Please wait</p>
        </div>
    );
}
