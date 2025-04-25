// entities/User.ts
import { formatTableName } from "@/lib/backend/utils";
import {
    Column,
    CreateDateColumn,
    Entity,
    OneToMany,
    PrimaryGeneratedColumn,
    UpdateDateColumn,
} from "typeorm";
import { Chat } from "./Chat";

@Entity({ name: formatTableName(User.name) })
export class User {
    @PrimaryGeneratedColumn("uuid")
    id!: string;

    @Column()
    address!: string;

    @Column()
    userType!: "contributor" | "guest";

    @Column({ nullable: true })
    userDID?: string;

    @OneToMany("Chat", "User")
    chats!: Chat[];

    @CreateDateColumn()
    createdAt!: Date;

    @UpdateDateColumn()
    lastLogin!: Date;
}
