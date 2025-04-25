"use client";
import { CHAIN_ID } from "@/lib/constants";
import {
    connectKeplr,
    getDidFromPubKeyBase64,
    getKeplr,
    verifyADR36Signature,
} from "@/lib/keplr/keplr";
import { signIn } from "next-auth/react";
import Image from "next/image";
import { useRouter } from "next/navigation";
import { useEffect, useState } from "react";
import styles from "./page.module.scss";
import { useAuth } from "@/hooks/useAuth";

export default function LoginPage() {
    const {
        setAddress,
        setUserType,
        setUserDID,
        initialized,
        isAuthenticated,
    } = useAuth({
        autoRedirect: false,
    });
    const router = useRouter();
    const [isLoading, setIsLoading] = useState(false);

    useEffect(() => {
        if (initialized === true && isAuthenticated === true) {
            router.push("/home");
        }
    }, [router, initialized, isAuthenticated]);

    const handleLogin = async () => {
        setIsLoading(true);
        try {
            const result = await connectKeplr();
            if (!result) return;
            const keplr = getKeplr();
            const message = `Sign in to DoctAI at ${new Date().toISOString()}`;
            const signed = await keplr.signArbitrary(
                CHAIN_ID,
                result.address,
                message
            );
            const pubKey = signed.pub_key.value;

            const userDID = getDidFromPubKeyBase64(pubKey);

            const isValid = verifyADR36Signature({
                message,
                address: result.address,
                pubKeyBase64: pubKey,
                signatureBase64: signed.signature,
            });

            if (!isValid) {
                alert("Signature verification failed. Please retry.");
                return;
            }

            const login = await signIn("credentials", {
                address: result.address,
                message,
                signature: signed.signature,
                pubKey,
                redirect: false,
            });

            if (login?.ok) {
                setAddress(result.address);
                setUserType("guest"); // provisional; el backend setea el real
                setUserDID(userDID);
                router.push("/home");
            } else {
                alert("Login failed");
            }
        } catch (error) {
            console.error(error);
        } finally {
            setIsLoading(false);
        }
    };

    return (
        <div className={styles.loginWrapper}>
            <div className={styles.loginBox}>
                <Image
                    src="/logo.png"
                    alt="DoctAI Logo"
                    width={250}
                    height={250}
                    priority
                />
                <p className={styles.subtitle}>
                    DoctAI is an AI-powered analysis platform for cardiac
                    datasets. Connect your wallet to get started.
                </p>
                <button onClick={handleLogin} className={styles.loginBtn}>
                    {isLoading ? "Connecting..." : "Connect Keplr Wallet"}
                </button>
            </div>
        </div>
    );
}
