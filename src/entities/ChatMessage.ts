// entities/ChatMessage.ts
import {
    Entity,
    PrimaryGeneratedColumn,
    Column,
    ManyToOne,
    CreateDateColumn,
    UpdateDateColumn,
    JoinColumn,
} from "typeorm";
import { Chat } from "./Chat";
import { formatTableName } from "@/lib/backend/utils";

@Entity({ name: formatTableName(ChatMessage.name) })
export class ChatMessage {
    @PrimaryGeneratedColumn("uuid")
    id!: string;

    @ManyToOne("Chat", "chatMessages")
    @JoinColumn({ name: "chatId" })
    chat!: Chat;

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
