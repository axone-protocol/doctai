"use client";
import { useEffect, useState } from "react";
import Link from "next/link";
import { useUserStore } from "@/store/useUserStore";
import { ROUTES } from "@/lib/routes";
import { Chat } from "@/entities/Chat";
import { fetchEntities } from "@/lib/frontend/api";
import styles from "./page.module.scss";

export default function ChatHistoryPage() {
    const { user } = useUserStore();
    const [chats, setChats] = useState<Chat[]>([]);

    useEffect(() => {
        if (!user) return;
        const loadChats = async () => {
            try {
                const chats = await fetchEntities<Chat>(Chat.name, {
                    filter: { userId: user.id },
                    sort: "createdAt",
                    order: "desc",
                });
                setChats(chats);
            } catch (error) {
                console.error("Failed to fetch chat history:", error);
            }
        };
        loadChats();
    }, [user]);

    return (
        <div className={styles.chatHistoryContainer}>
            <h1>Past Chats</h1>
            <ul className={styles.chatList}>
                {chats.map((chat) => (
                    <li key={chat.id} className={styles.chatItem}>
                        <Link
                            href={`${ROUTES.CHAT_INTERACTION}/${chat.id}`}
                            className={styles.chatLink}
                        >
                            {chat.title || "Untitled Chat"} <br />
                            <small>
                                {new Date(chat.createdAt).toLocaleString()}
                            </small>
                        </Link>
                        <Link
                            href={`${ROUTES.CHAT_HISTORY_DETAILS}/${chat.id}`}
                            className={styles.viewLink}
                        >
                            View
                        </Link>
                    </li>
                ))}
            </ul>
        </div>
    );
}
