"use client";

import { useEffect, useState } from "react";
import { Smile } from "lucide-react";
import { useUserStore } from "@/store/useUserStore";
import { fetchWalletBalance } from "@/lib/cosmos/frontend/keplr";
import styles from "./page.module.scss";

export default function HomePage() {
    const { address, userType } = useUserStore();
    const [balance, setBalance] = useState<number | null>(null);

    useEffect(() => {
        const loadBalance = async () => {
            if (address) {
                const result = await fetchWalletBalance(address);
                setBalance(result);
            }
        };
        loadBalance();
    }, [address]);

    return (
        <div className={styles.homeContainer}>
            <div className={styles.card}>
                <div className={styles.cardIcon}>
                    <Smile />
                </div>
                <div className={styles.cardContent}>
                    <h2 className={styles.cardTitle}>Welcome to DoctAI!</h2>
                    <p className={styles.cardText}>
                        We&apos;re glad to have you here. Your connected wallet:
                        <br />
                        <strong>{address}</strong>
                    </p>
                    <p className={styles.cardText}>
                        User type: <strong>{userType}</strong>
                    </p>
                    {balance !== null && (
                        <p className={styles.cardText}>
                            Wallet balance:{" "}
                            <strong>{balance.toFixed(6)} AXONE</strong>
                        </p>
                    )}
                </div>
            </div>
        </div>
    );
}
