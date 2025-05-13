// entities/ChatMessage.ts
import { ENTITY_NAMES } from "@/lib/constants";
import {
    Column,
    CreateDateColumn,
    Entity,
    JoinColumn,
    ManyToOne,
    PrimaryGeneratedColumn,
    UpdateDateColumn,
} from "typeorm";
import { type IChat, IChatMessage } from "./types";

@Entity({ name: ENTITY_NAMES.ChatMessage })
export class ChatMessage implements IChatMessage {
    @PrimaryGeneratedColumn("uuid")
    id!: string;

    @ManyToOne("Chat", "chatMessages")
    @JoinColumn({ name: "chatId" })
    chat!: IChat;

    @Column()
    chatId!: string;

    @Column()
    role!: "user" | "assistant";

    @Column("text")
    content!: string;

    @CreateDateColumn()
    createdAt!: Date;

    @UpdateDateColumn()
    updatedAt!: Date;
}

Object.defineProperty(ChatMessage, "name", { value: ENTITY_NAMES.ChatMessage });