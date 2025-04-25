"use client";
import { useEffect, useState } from "react";
import { useUserStore } from "@/store/useUserStore";
import { createEntity } from "@/lib/frontend/api";
import styles from "./page.module.scss";
import { Chat } from "@/entities/Chat";

interface Message {
    id: string;
    role: "user" | "assistant";
    content: string;
    createdAt?: string;
}

export default function ChatPage() {
    const { address, user } = useUserStore();
    const [chatId, setChatId] = useState<string | undefined>(undefined);
    const [messages, setMessages] = useState<Message[]>([]);
    const [input, setInput] = useState("");
    const [loading, setLoading] = useState(false);
    const [hasCreatedChat, setHasCreatedChat] = useState(false);

    useEffect(() => {
        if (!user?.id || hasCreatedChat) return;
        (async () => {
            const chat = await createEntity<Chat>(Chat.name, {
                title: "New Chat",
                userId: user.id,
            });
            setChatId(chat.id);
            setHasCreatedChat(true);
        })();
    }, [user?.id, hasCreatedChat]);

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
            <div className={styles.messages}>
                {messages.map((msg) => (
                    <div
                        key={msg.id}
                        className={`${styles.message} ${
                            msg.role === "user" ? styles.user : styles.assistant
                        }`}
                    >
                        <strong>
                            {msg.role === "user" ? "You" : "DoctAI"}
                        </strong>
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
