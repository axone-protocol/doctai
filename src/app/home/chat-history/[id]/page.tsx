"use client";
import { useEffect, useState } from "react";
import Link from "next/link";
import { useParams } from "next/navigation";
import { fetchEntityById, fetchEntityRelation } from "@/lib/frontend/api";
import { Chat } from "@/entities/Chat";
import { ChatMessage } from "@/entities/ChatMessage";
import { getRelationKey } from "@/lib/relations";
import { ROUTES } from "@/lib/routes";
import styles from "./page.module.scss";

export default function ChatHistoryDetailPage() {
    const { id } = useParams();
    const [messages, setMessages] = useState<ChatMessage[]>([]);
    const [datasetUrl, setDatasetUrl] = useState<string | null>(null);

    useEffect(() => {
        if (!id) return;

        const loadMessages = async () => {
            try {
                const msgs = await fetchEntityRelation<ChatMessage>(
                    Chat.name,
                    id as string,
                    getRelationKey(Chat, ChatMessage)
                );
                setMessages(msgs);
            } catch (error) {
                console.error("Failed to fetch chat messages:", error);
            }
        };

        const loadChatInfo = async () => {
            try {
                const chat = await fetchEntityById<Chat>(Chat.name, id as string);
                setDatasetUrl(chat.datasetUrl || null);
            } catch (error) {
                console.error("Failed to fetch chat info:", error);
            }
        };

        loadMessages();
        loadChatInfo();
    }, [id]);

    return (
        <div className={styles.chatContainer}>
            <Link href={ROUTES.CHAT_HISTORY} className={styles.backLink}>
                ← Back to chat history
            </Link>

            <h1>Chat Preview</h1>

            {datasetUrl && (
                <div className={styles.datasetInfo}>
                    <strong>Dataset:</strong>{" "}
                    <a href={datasetUrl} target="_blank" rel="noopener noreferrer">
                        {datasetUrl}
                    </a>
                </div>
            )}

            <div className={styles.messages}>
                {messages.map((msg) => (
                    <div
                        key={msg.id}
                        className={`${styles.message} ${
                            msg.role === "user" ? styles.user : styles.assistant
                        }`}
                    >
                        <strong>{msg.role === "user" ? "You" : "DoctAI"}</strong>
                        {msg.content}
                        <br />
                        <small>{new Date(msg.createdAt).toLocaleString()}</small>
                    </div>
                ))}
            </div>

            <Link
                href={`${ROUTES.CHAT_INTERACTION}/${id}`}
                className={styles.backLink}
            >
                👉 Continue this chat
            </Link>
        </div>
    );
}
