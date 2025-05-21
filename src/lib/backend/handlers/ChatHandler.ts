// lib/backend/handlers/ChatHandler.ts
import { Chat } from "@/entities/Chat";
import { BaseEntityServer, registerEntity } from "../BaseEntityServer";
import { ENTITY_NAMES } from "@/lib/constants";

export const ChatHandler: BaseEntityServer<Chat> = {
    entityName: ENTITY_NAMES.Chat,

    toDTO: (chat) => ({
        id: chat.id,
        title: chat.title,
        userId: chat.userId,
        datasetUrl: chat.datasetUrl,
        datasetDID: chat.datasetDID,
        paymentTxHash: chat.paymentTxHash,
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
        chat.paymentTxHash = dto.paymentTxHash;
        chat.datasetCredentialTxHash = dto.datasetCredentialTxHash;
        // chat.createdAt = new Date(dto.createdAt);
        // chat.updatedAt = new Date(dto.updatedAt);
        return chat;
    },
};
registerEntity(ChatHandler);
