// lib/backend/handlers/ChatMessageHandler.ts
import { ChatMessage } from "@/entities/ChatMessage";
import { BaseEntityServer, registerEntity } from "../BaseEntityServer";
import { ENTITY_NAMES } from "@/lib/constants";

export const ChatMessageHandler: BaseEntityServer<ChatMessage> = {
    entityName: ENTITY_NAMES.ChatMessage,

    toDTO: (msg) => ({
        id: msg.id,
        chatId: msg.chatId,
        role: msg.role,
        content: msg.content,
        createdAt: msg.createdAt.toISOString(),
        updatedAt: msg.updatedAt.toISOString(),
    }),

    fromDTO: (dto) => {
        const msg = new ChatMessage();
        msg.id = dto.id;
        msg.chatId = dto.chatId;
        msg.role = dto.role;
        msg.content = dto.content;
        // msg.createdAt = new Date(dto.createdAt);
        // msg.updatedAt = new Date(dto.updatedAt);
        return msg;
    },
};

registerEntity(ChatMessageHandler);
