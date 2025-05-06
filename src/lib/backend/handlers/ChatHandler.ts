// lib/backend/handlers/ChatHandler.ts
import { Chat } from "@/entities/Chat";
import { BaseEntityServer, registerEntity } from "../BaseEntityServer";

export const ChatHandler: BaseEntityServer<Chat> = {
    entityName: Chat.name,

    toDTO: (chat) => ({
        id: chat.id,
        title: chat.title,
        userId: chat.userId,
        datasetUrl: chat.datasetUrl,
        datasetDID: chat.datasetDID,
        datasetCredentialTxHash: chat.datasetCredentialTxHash,
        createdAt: chat.createdAt.toISOString(),
        updatedAt: chat.updatedAt.toISOString(),
    }),

    fromDTO: (dto) => {
        const chat = new Chat();
        chat.id = dto.id;
        chat.title = dto.title;
        chat.userId = dto.userId;
        chat.datasetUrl = dto.datasetUrl;
        chat.datasetDID = dto.datasetDID;
        chat.datasetCredentialTxHash = dto.datasetCredentialTxHash;
        // chat.createdAt = new Date(dto.createdAt);
        // chat.updatedAt = new Date(dto.updatedAt);
        return chat;
    },
};
registerEntity(ChatHandler);
