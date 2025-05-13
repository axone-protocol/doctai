// entities/User.ts
import { ENTITY_NAMES } from "@/lib/constants";
import {
    Column,
    CreateDateColumn,
    Entity,
    OneToMany,
    PrimaryGeneratedColumn,
    UpdateDateColumn,
} from "typeorm";
import { IChat, IUser } from "./types";

@Entity({ name: ENTITY_NAMES.User })
export class User implements IUser {
    @PrimaryGeneratedColumn("uuid")
    id!: string;

    @Column()
    address!: string;

    @Column()
    userType!: "contributor" | "guest";

    @Column({ nullable: true })
    userDID?: string;

    @OneToMany("Chat", "user")
    chats!: IChat[];

    @CreateDateColumn()
    createdAt!: Date;

    @UpdateDateColumn()
    lastLogin!: Date;
}

Object.defineProperty(User, "name", { value: ENTITY_NAMES.User });
