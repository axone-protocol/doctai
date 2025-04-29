"use client";

import {
    HEARTH_LABS_ZONE_DID
} from "@/lib/constants";
import { fetchPrologFromAddress, fetchZoneGovAddress } from "@/lib/keplr/utils";
import { useEffect, useState } from "react";
import styles from "./page.module.scss";

export default function ZonePage() {
    const [zoneGovAddress, setZoneGovAddress] = useState<string>("loading...");
    const [prologCode, setPrologCode] = useState<string>("");

    const [loading, setLoading] = useState<boolean>(true);

    useEffect(() => {
        async function loadZoneGovernance() {
            try {
                const govAddr = await fetchZoneGovAddress(HEARTH_LABS_ZONE_DID);
                setZoneGovAddress(govAddr);

                const prolog = await fetchPrologFromAddress(govAddr);
                setPrologCode(prolog);
            } catch (error) {
                console.error("Error fetching governance or prolog:", error);
                setZoneGovAddress("error");
                setPrologCode("failed to load");
            } finally {
                setLoading(false);
            }
        }

        loadZoneGovernance();
    }, []);

    return (
        <>
            <h1>Zone Hearts Labs</h1>
            <p>Zone DID: {HEARTH_LABS_ZONE_DID}</p>
            <p>Zone Gov Address: {loading ? "loading..." : zoneGovAddress}</p>
            <p>Zone Gov Code: </p>
            <pre className={styles.code}>
                {loading ? "loading..." : prologCode}
            </pre>
        </>
    );
}
