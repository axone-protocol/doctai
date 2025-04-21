"use client";
import { useUserStore } from "@/store/useUserStore";

export default function RewardsPage() {
    const { address, userType } = useUserStore();

    return (
        <>
            <h1>Rewards</h1>
            <p>Wallet: {address}</p>
            <p>User type: {userType}</p>
        </>
    );
}
