"use client";

import { useEffect, useState } from "react";
import { useUserStore } from "@/store/useUserStore";
import {
    HEARTH_LABS_ZONE_DID,
    MINIO_DID,
    HEARTH_LABS_ZONES_GOV_ADDRESS,
} from "@/lib/constants";
import { fetchZoneGovAddress } from "@/lib/keplr/utils";

export default function HomePage() {
    const { address, userType } = useUserStore();

    const [zoneGovAddress, setZoneGovAddress] = useState<string>("loading...");
    const [loading, setLoading] = useState<boolean>(true);

    useEffect(() => {
        async function loadZoneGovernance() {
            try {
                const address = await fetchZoneGovAddress(HEARTH_LABS_ZONE_DID);
                setZoneGovAddress(address);
            } catch (error) {
                console.error("Failed to fetch zone governance address:", error);
                setZoneGovAddress("error");
            } finally {
                setLoading(false);
            }
        }

        loadZoneGovernance();
    }, []);

    return (
        <>
            <h1>Welcome to DoctAI</h1>
            <p>Wallet: {address}</p>
            <p>User type: {userType}</p>
            <p>Zone DID: {HEARTH_LABS_ZONE_DID}</p>
            <p>Minio DID: {MINIO_DID}</p>
            <p>Zone Gov Address: {loading ? "loading..." : zoneGovAddress}</p>
        </>
    );
}
