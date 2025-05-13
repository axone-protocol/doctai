// app/api/entities/chat/[id]/interact/route.ts
import { Chat } from "@/entities/Chat";
import { ChatMessage } from "@/entities/ChatMessage";
import { PostgreSQLDatabaseService } from "@/lib/backend/PostgreSQLDatabaseService";
import { ENTITY_NAMES } from "@/lib/constants";
import { getRelationPropertyName } from "@/lib/relations";
import { NextRequest, NextResponse } from "next/server";

export async function POST(
    request: NextRequest,
    { params }: { params: Promise<{ id: string }> }
) {
    const resolvedParams = await params;
    const chatId = resolvedParams.id;
    const db = await PostgreSQLDatabaseService.getDataSource();
    const repoChat = db.getRepository(Chat);
    const repoMessage = db.getRepository(ChatMessage);
    try {
        console.log("[DEBUG] ENTITY_NAMES", ENTITY_NAMES);
        console.log("[DEBUG] Chat entity", Chat);
        console.log(
            "[DEBUG] getRelationPropertyName(Chat, User)",
            getRelationPropertyName(ENTITY_NAMES.Chat, ENTITY_NAMES.User)
        );
        const chat = await repoChat.findOne({
            where: { id: chatId },
            relations: [
                getRelationPropertyName(ENTITY_NAMES.Chat, ENTITY_NAMES.User),
            ],
        });
        if (!chat) {
            return NextResponse.json(
                { error: "Chat not found" },
                { status: 404 }
            );
        }
        const body = await request.json();
        const prompt = body?.prompt?.trim();
        if (!prompt) {
            return NextResponse.json(
                { error: "Missing prompt" },
                { status: 400 }
            );
        }
        // Save user message
        const userMessage = repoMessage.create({
            chatId,
            role: "user",
            content: prompt,
        });
        await repoMessage.save(userMessage);
        // Simulate assistant response
        const assistantResponse = `This is a dummy answer to: "${prompt}"`;
        const assistantMessage = repoMessage.create({
            chatId,
            role: "assistant",
            content: assistantResponse,
        });
        await repoMessage.save(assistantMessage);
        return NextResponse.json({
            userMessageId: userMessage.id,
            assistantMessageId: assistantMessage.id,
            assistantResponse,
        });
    } catch (error) {
        console.error("[Chat Interact Error]", error);
        return NextResponse.json(
            { error: "Internal Server Error" },
            { status: 500 }
        );
    }
}
