"use client";
import { useEffect, useState } from "react";
import Link from "next/link";
import { useUserStore } from "@/store/useUserStore";
import { ROUTES } from "@/lib/routes";
import { Chat } from "@/entities/Chat";
import { fetchEntities } from "@/lib/frontend/api";


export default function ChatHistoryPage() {
    const { address, user } = useUserStore();
    const [chats, setChats] = useState<Chat[]>([]);

    useEffect(() => {
        if (user === undefined) return;
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
        <div>
            <h1>Chat History</h1>
            <ul>
                {chats.map((chat) => (
                    <li key={chat.id}>
                        <Link href={`${ROUTES.CHAT}/${chat.id}`}>
                            {chat.title || "Untitled Chat"} — {new Date(chat.createdAt).toLocaleString()}
                        </Link>
                    </li>
                ))}
            </ul>
        </div>
    );
}
