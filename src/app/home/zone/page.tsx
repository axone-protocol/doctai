"use client";
import { useUserStore } from "@/store/useUserStore";

export default function ZonePage() {
    const { address, userType } = useUserStore();

    return (
        <>
            <h1>Hearth Labs Zone</h1>
            <p>Wallet: {address}</p>
            <p>User type: {userType}</p>
        </>
    );
}
