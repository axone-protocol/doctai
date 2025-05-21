"use client";

import { useEffect, useState } from "react";
import { useParams } from "next/navigation";
import { useUserStore } from "@/store/useUserStore";
import { fetchEntityById, fetchEntityRelation } from "@/lib/frontend/api";
import { Chat } from "@/entities/Chat";
import { ChatMessage } from "@/entities/ChatMessage";
import { getRelationKey } from "@/lib/relations";
import styles from "./page.module.scss";
import { fetchFullCredentialByDID } from "@/lib/cosmos/frontend/axone";
import { Loader } from "lucide-react";
import { ENTITY_NAMES } from "@/lib/constants";

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
    const [isLoadingInfo, setIsLoadingInfo] = useState(true);
    const [chat, setChat] = useState<Chat | null>(null);
    const [datasetCredential, setDatasetCredential] = useState<Record<
        string,
        any
    > | null>(null);

    useEffect(() => {
        if (!chatId) return;

        const loadAll = async () => {
            try {
                const msgs = await fetchEntityRelation<ChatMessage>(
                    ENTITY_NAMES.Chat,
                    chatId as string,
                    getRelationKey(ENTITY_NAMES.Chat, ENTITY_NAMES.ChatMessage)
                );

                const formatted = msgs.map((msg) => ({
                    id: msg.id,
                    role: msg.role,
                    content: msg.content,
                    createdAt: msg.createdAt?.toLocaleString(),
                }));

                setMessages(formatted);

                const chat = await fetchEntityById<Chat>(
                    ENTITY_NAMES.Chat,
                    chatId as string
                );
                setChat(chat);

                if (chat.datasetDID) {
                    const cred = await fetchFullCredentialByDID(
                        chat.datasetDID
                    );
                    setDatasetCredential(cred);
                }
            } catch (error) {
                console.error(
                    "Failed to fetch chat or dataset credential:",
                    error
                );
            } finally {
                setIsLoadingInfo(false);
            }
        };

        loadAll();
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

            {isLoadingInfo ? (
                <div className={styles.loading}>
                    <div className={styles.topRow}>
                        <span className={styles.icon}>
                            <Loader />
                        </span>
                        <span className={styles.dots}>
                            <span>.</span>
                            <span>.</span>
                            <span>.</span>
                        </span>
                    </div>
                    <p className={styles.loadingText}>
                        Loading Detaset Data...
                    </p>
                </div>
            ) : (
                <>
                    {chat?.datasetUrl && (
                        <div className={styles.datasetInfo}>
                            <strong>Dataset URL:</strong>{" "}
                            <a
                                href={chat.datasetUrl}
                                target="_blank"
                                rel="noopener noreferrer"
                            >
                                {chat.datasetUrl}
                            </a>
                        </div>
                    )}

                    {chat?.datasetDID && (
                        <div className={styles.datasetInfo}>
                            <strong>Dataset DID:</strong> {chat.datasetDID}
                        </div>
                    )}

                    {chat?.paymentTxHash && (
                        <div className={styles.datasetInfo}>
                            <strong>Dataset paymentTxHash:</strong> {chat.paymentTxHash}
                        </div>
                    )}

                    {chat?.datasetCredentialTxHash && (
                        <div className={styles.datasetInfo}>
                            <strong>Dataset datasetCredentialTxHash:</strong> {chat.datasetCredentialTxHash}
                        </div>
                    )}

                    {datasetCredential && (
                        <div className={styles.credentialBlock}>
                            <strong>Dataset Credential</strong>
                            <pre>
                                {JSON.stringify(datasetCredential, null, 2)}
                            </pre>
                        </div>
                    )}
                </>
            )}

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
