"use client";
import { useUserStore } from "@/store/useUserStore";

export default function HomePage() {
    const { address, userType } = useUserStore();

    return (
        <>
            <h1>Welcome to DoctAI</h1>
            <p>Wallet: {address}</p>
            <p>User type: {userType}</p>
        </>
    );
}
