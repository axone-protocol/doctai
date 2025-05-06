"use client";
import { useEffect, useState } from "react";
import { useParams } from "next/navigation";
import { useUserStore } from "@/store/useUserStore";
import { fetchEntityById, fetchEntityRelation } from "@/lib/frontend/api";
import { Chat } from "@/entities/Chat";
import { ChatMessage } from "@/entities/ChatMessage";
import { getRelationKey } from "@/lib/relations";
import styles from "./page.module.scss";

interface Message {
    id: string;
    role: "user" | "assistant";
    content: string;
    createdAt?: string;
}

export default function ChatInteractPage() {
    const { id: chatId } = useParams();
    const { user } = useUserStore();
    const [messages, setMessages] = useState<Message[]>([]);
    const [input, setInput] = useState("");
    const [loading, setLoading] = useState(false);
    const [datasetUrl, setDatasetUrl] = useState<string | null>(null);

    useEffect(() => {
        if (!chatId) return;

        const loadMessages = async () => {
            try {
                const msgs = await fetchEntityRelation<ChatMessage>(
                    Chat.name,
                    chatId as string,
                    getRelationKey(Chat, ChatMessage)
                );

                const formatted = msgs.map((msg) => ({
                    id: msg.id,
                    role: msg.role,
                    content: msg.content,
                    createdAt: msg.createdAt.toLocaleString(),
                }));

                setMessages(formatted);
            } catch (error) {
                console.error("Failed to fetch chat messages:", error);
            }
        };

        const loadChatInfo = async () => {
            try {
                const chat = await fetchEntityById<Chat>(Chat.name, chatId as string);
                setDatasetUrl(chat.datasetUrl || null);
            } catch (error) {
                console.error("Failed to fetch chat info:", error);
            }
        };

        loadMessages();
        loadChatInfo();
    }, [chatId]);

    const sendMessage = async () => {
        if (!input.trim() || !chatId) return;
        setLoading(true);

        const userMsg: Message = {
            id: crypto.randomUUID(),
            role: "user",
            content: input,
        };
        setMessages((prev) => [...prev, userMsg]);
        setInput("");

        const res = await fetch(`/api/entities/chat/${chatId}/interact`, {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ prompt: userMsg.content }),
        });

        const data = await res.json();

        if (data.assistantResponse) {
            const assistantMsg: Message = {
                id: crypto.randomUUID(),
                role: "assistant",
                content: data.assistantResponse,
            };
            setMessages((prev) => [...prev, assistantMsg]);
        }

        setLoading(false);
    };

    return (
        <div className={styles.chatContainer}>
            <h1>Chat with DoctAI</h1>

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
                    </div>
                ))}
            </div>

            <div className={styles.inputContainer}>
                <input
                    className={styles.inputField}
                    type="text"
                    placeholder="Type your question..."
                    value={input}
                    onChange={(e) => setInput(e.target.value)}
                    onKeyDown={(e) => e.key === "Enter" && sendMessage()}
                />
                <button
                    className={styles.sendButton}
                    onClick={sendMessage}
                    disabled={loading}
                >
                    {loading ? "Sending..." : "Send"}
                </button>
            </div>
        </div>
    );
}
