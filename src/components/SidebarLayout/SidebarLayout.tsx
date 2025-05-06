// components/SidebarLayout/SidebarLayout.tsx
"use client";

import { useAuth } from "@/hooks/useAuth";
import { useResponsive } from "@/hooks/useResponsive"; // importamos el hook nuevo
import { signOut } from "next-auth/react";
import Link from "next/link";
import { usePathname, useRouter } from "next/navigation";
import { useState } from "react";
import styles from "./SidebarLayout.module.scss";
import { ROUTES } from "@/lib/routes";

export default function SidebarLayout({
    children,
}: {
    children: React.ReactNode;
}) {
    const { address, userType, reset } = useAuth();
    const pathname = usePathname();
    const router = useRouter();
    const [sidebarOpen, setSidebarOpen] = useState(false);
    const { isMobile } = useResponsive(); // usamos el hook

    const toggleSidebar = () => {
        setSidebarOpen(!sidebarOpen);
    };

    const handleLogout = async () => {
        await signOut({ redirect: false });
        reset();
        router.push("/login");
    };

    // Link definitions for both contributor and guest
    const contributorLinks = [
        { href: ROUTES.CHAT, label: "Chat with DoctAI" },
        { href: ROUTES.CHAT_HISTORY, label: "Chats History" },
        { href: ROUTES.UPLOAD, label: "Upload Dataset" },
        { href: ROUTES.REWARDS, label: "Rewards" },
        { href: ROUTES.GOVERNANCE, label: "Governance" },
        { href: ROUTES.ZONE, label: "Zone" },
    ];
    
    const guestLinks = [
        { href: ROUTES.CHAT, label: "Chat with DoctAI" },
        { href: ROUTES.CHAT_HISTORY, label: "Chats History" },
        { href: ROUTES.PAYMENTS, label: "Payment History" },
        { href: ROUTES.GOVERNANCE, label: "Governance" },
        { href: ROUTES.ZONE, label: "Zone" },
    ];
    // Render a generic menu
    const renderMenu = (links: { href: string; label: string }[]) => (
        <>
            {links.map((link) => (
                <Link
                    key={link.href}
                    href={link.href}
                    className={`${styles.navLink} ${
                        pathname === link.href ? styles.active : ""
                    }`}
                    onClick={() => isMobile && setSidebarOpen(false)}
                >
                    {link.label}
                </Link>
            ))}
        </>
    );

    return (
        <div className={styles.container}>
            {isMobile && (
                <button
                    className={styles.toggleButton}
                    onClick={toggleSidebar}
                    aria-label="Toggle sidebar"
                >
                    ☰
                </button>
            )}

            <div
                className={`${styles.sidebar} ${
                    sidebarOpen ? styles.open : ""
                }`}
            >
                <nav className={styles.nav}>
                    {userType === "contributor"
                        ? renderMenu(contributorLinks)
                        : renderMenu(guestLinks)}
                </nav>
            </div>

            <main
                className={`${styles.main} ${
                    sidebarOpen && isMobile ? styles.pushed : ""
                }`}
            >
                {children}
                {address && (
                    <button
                        className={styles.logoutButton}
                        onClick={handleLogout}
                    >
                        Logout
                    </button>
                )}
            </main>

            {sidebarOpen && isMobile && (
                <div className={styles.overlay} onClick={toggleSidebar} />
            )}
        </div>
    );
}
