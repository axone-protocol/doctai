"use client";
import { useUserStore } from "@/store/useUserStore";

export default function PaymentPage() {
    const { address, userType } = useUserStore();

    return (
        <>
            <h1>Payment History</h1>
            <p>Wallet: {address}</p>
            <p>User type: {userType}</p>
        </>
    );
}
