"use client";
import { useUserStore } from "@/store/useUserStore";

export default function UploadPage() {
    const { address, userType } = useUserStore();

    return (
        <>
            <h1>Upload Dataset</h1>
            <p>Wallet: {address}</p>
            <p>User type: {userType}</p>
        </>
    );
}
