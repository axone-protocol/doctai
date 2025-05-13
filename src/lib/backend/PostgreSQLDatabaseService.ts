// lib/backend/PostgreSQLDatabaseService.ts
import { Chat } from "@/entities/Chat";
import { ChatMessage } from "@/entities/ChatMessage";
import { User } from "@/entities/User";
import "reflect-metadata";
import { DataSource } from "typeorm";
import { swLogsDBDebug } from "../constants";

let dataSource: DataSource | undefined = undefined;
let connectPromise: Promise<DataSource> | undefined = undefined;

export class PostgreSQLDatabaseService {
    public static async getDataSource(): Promise<DataSource> {
        // Return existing connection if initialized
        if (dataSource?.isInitialized) return dataSource;

        // Return existing connection promise if in progress
        if (connectPromise !== undefined) return connectPromise;

        // Create new connection promise
        connectPromise = (async () => {
            try {
                // First, check if database exists and create it if not
                const defaultSource = new DataSource({
                    type: "postgres",
                    host: process.env.STORAGE_POSTGRES_HOST,
                    port: Number(process.env.STORAGE_POSTGRES_PORT),
                    username: process.env.STORAGE_POSTGRES_USER,
                    password: process.env.STORAGE_POSTGRES_PASSWORD,
                    database: process.env.STORAGE_POSTGRES_DATABASE,
                    entities: [],
                    synchronize: false,
                    logging: swLogsDBDebug
                        ? ["query", "error", "schema", "warn"]
                        : ["error"],
                });

                await defaultSource.initialize();
                console.log(
                    "[DB] Connected to default database to check if app database exists"
                );

                const runner = defaultSource.createQueryRunner();
                await runner.connect();

                const dbName = process.env.STORAGE_POSTGRES_DATABASE;
                const exists = await runner.query(
                    `SELECT 1 FROM pg_database WHERE datname = $1`,
                    [dbName]
                );

                if (exists.length === 0) {
                    console.log(`[DB] Creating database "${dbName}"`);
                    await runner.query(`CREATE DATABASE "${dbName}"`);
                } else {
                    console.log(`[DB] Database "${dbName}" already exists`);
                }

                await runner.release();
                await defaultSource.destroy();

                // Now connect to the application database
                const appSource = new DataSource({
                    type: "postgres",
                    host: process.env.STORAGE_POSTGRES_HOST,
                    port: Number(process.env.STORAGE_POSTGRES_PORT),
                    username: process.env.STORAGE_POSTGRES_USER,
                    password: process.env.STORAGE_POSTGRES_PASSWORD,
                    database: process.env.STORAGE_POSTGRES_DATABASE,
                    entities: [Chat, ChatMessage, User],
                    synchronize: true, // auto sync schema
                    logging: swLogsDBDebug
                        ? ["query", "error", "schema", "warn"]
                        : ["error"],
                });

                dataSource = await appSource.initialize();
                console.log("[DB] PostgreSQL connected and ready");
                return dataSource;
            } catch (error) {
                console.error("[PostgreSQL] Connection error:", error);
                dataSource = undefined;
                connectPromise = undefined;
                throw error;
            }
        })();

        return connectPromise;
    }
}
