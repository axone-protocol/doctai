// app/layout.tsx
import "./globals.scss";
import type { Metadata } from "next";
import SessionProvider from "@/components/SessionProvider/SessionProvider";
import styles from "./layout.module.scss"; // NUEVO

export const metadata: Metadata = {
    title: "DoctAI",
    description: "Decentralized AI Health Assistant",
    icons: {
        icon: "/favicon.ico",
        apple: "/apple-touch-icon.png",
    },
    manifest: "/manifest.json",
};

export default function RootLayout({
    children,
}: {
    children: React.ReactNode;
}) {
    return (
        <html lang="en">
            <body>
                <SessionProvider>
                    <div className={styles.wrapper}>
                        <main className={styles.mainContent}>{children}</main>
                        <footer className={styles.footer}>
                            Need help? Contact us at{" "}
                            <a href="mailto:contact@axone.xyz">
                                contact@axone.xyz
                            </a>
                        </footer>
                    </div>
                </SessionProvider>
            </body>
        </html>
    );
}
