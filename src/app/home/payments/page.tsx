"use client";
import { useUserStore } from "@/store/useUserStore";
import styles from "./page.module.scss";

export default function PaymentPage() {
    const { address, userType } = useUserStore();

    return (
        <div className={styles.pageContainer}>
            <h1>Payment History</h1>
            <p>Wallet: {address}</p>
            <p>User type: {userType}</p>
        </div>
    );
}
