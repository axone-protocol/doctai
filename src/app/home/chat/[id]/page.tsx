"use client";
import { useEffect, useState } from "react";
import { useParams } from "next/navigation";
import { fetchEntityRelation } from "@/lib/frontend/api";
import { Chat } from "@/entities/Chat";
import { ChatMessage } from "@/entities/ChatMessage";
import { getRelationKey } from "@/lib/relations";

export default function ChatDetailPage() {
    const { id } = useParams();
    const [messages, setMessages] = useState<ChatMessage[]>([]);

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

        loadMessages();
    }, [id]);

    return (
        <div>
            <h1>Chat Detail</h1>
            {messages.map((msg) => (
                <div key={msg.id}>
                    <strong>{msg.role}</strong>: {msg.content}
                    <br />
                    <small>{new Date(msg.createdAt).toLocaleString()}</small>
                </div>
            ))}
        </div>
    );
}
