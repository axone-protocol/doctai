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

    @ManyToOne("User", "Chat")
    @JoinColumn({ name: "userId" })
    User!: User;

    @Column()
    userId!: string;

    @OneToMany("ChatMessage", "Chat", { cascade: true })
    ChatMessage!: ChatMessage[];

    @CreateDateColumn()
    createdAt!: Date;

    @UpdateDateColumn()
    updatedAt!: Date;
}
