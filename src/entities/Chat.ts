// entities/Chat.ts
import { formatTableName } from "@/lib/backend/utils";
import {
    Column,
    CreateDateColumn,
    Entity,
    JoinColumn,
    ManyToOne,
    OneToMany,
    PrimaryGeneratedColumn,
    UpdateDateColumn,
} from "typeorm";
import { ChatMessage } from "./ChatMessage";
import { User } from "./User";

@Entity({ name: formatTableName(Chat.name) })
export class Chat {
    @PrimaryGeneratedColumn("uuid")
    id!: string;

    @Column()
    title!: string;

    @ManyToOne("User", "chat")
    @JoinColumn({ name: "userId" })
    user!: User;

    @Column()
    userId!: string;

    @Column({ nullable: true })
    datasetUrl?: string;

    @OneToMany("ChatMessage", "chat", { cascade: true })
    chatMessages!: ChatMessage[];

    @CreateDateColumn()
    createdAt!: Date;

    @UpdateDateColumn()
    updatedAt!: Date;
}
