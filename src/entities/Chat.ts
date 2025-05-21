// entities/Chat.ts
import { ENTITY_NAMES } from "@/lib/constants";
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
import { IChat, IChatMessage, type IUser } from "./types";

@Entity({ name: ENTITY_NAMES.Chat })
export class Chat implements IChat {
    @PrimaryGeneratedColumn("uuid")
    id!: string;

    @Column()
    title!: string;

    @ManyToOne("User", "chats")
    @JoinColumn({ name: "userId" })
    user!: IUser;

    @Column()
    userId!: string;

    @Column({ nullable: true })
    datasetUrl?: string;

    @Column({ nullable: true })
    datasetDID?: string;

    @Column({ nullable: true })
    paymentTxHash?: string;

    @Column({ nullable: true })
    datasetCredentialTxHash?: string;

    @OneToMany("ChatMessage", "chat", { cascade: true })
    chatMessages!: IChatMessage[];

    @Column({ default: "pending" })
    status!: string;

    @CreateDateColumn()
    createdAt!: Date;

    @UpdateDateColumn()
    updatedAt!: Date;
}

Object.defineProperty(Chat, "name", { value: ENTITY_NAMES.Chat });
