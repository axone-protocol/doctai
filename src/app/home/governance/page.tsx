"use client";
import { useUserStore } from "@/store/useUserStore";

export default function GovernancePage() {
    const { address, userType } = useUserStore();

    return (
        <>
            <h1>Governance</h1>
            <p>Wallet: {address}</p>
            <p>User type: {userType}</p>
        </>
    );
}
