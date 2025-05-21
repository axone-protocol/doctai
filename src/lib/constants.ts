//------------------------------------------------------------------
// AXONE

export const CHAIN_ID = process.env.NEXT_PUBLIC_CHAIN_ID || "axone-dentrite-1";

export const AXONED_PATH =
    process.env.NEXT_PUBLIC_AXONED_PATH ||
    "/home/manuelpadilla/sources/reposUbuntu/AXONE/tools/axoned-10.0.0-linux-amd64/axoned";

export const AXONE_NODE_RPC =
    process.env.NEXT_PUBLIC_AXONE_NODE_RPC ||
    "https://api.dentrite.axone.xyz:443/rpc";

export const AXONE_NODE_GRPC =
    process.env.NEXT_PUBLIC_AXONE_NODE_GRPC ||
    "axone-testnet-grpc.polkachu.com:17690";

export const MINIO_DID =
    process.env.NEXT_PUBLIC_MINIO_DID ||
    "did:key:zQ3shgHvy6b16afQRDqyUb94xAhU7wTEqLPygqCFz1yE2KtMA";


export const MINIO_WALLET_MNEMONIC_PHRASE =
    process.env.MINIO_WALLET_MNEMONIC_PHRASE ||
    "actress outer equal gain brisk ship dawn patch sock culture junior wise banner train panic demise nothing eye tortoise north vibrant people gym shed";

// export const HEARTH_LABS_ZONE_DID =
//     process.env.NEXT_PUBLIC_HEARTH_LABS_ZONE_DID ||
//     "did:key:zQ3shmDoRRiFmNzwDrnV2iPdgA9xaoAYsSq8GC9tGHsNL4R7a";

// export const HEARTH_LABS_ZONE_GOV_CODE_ADDRESS =
//     process.env.NEXT_PUBLIC_HEARTH_LABS_ZONE_GOV_CODE_ADDRESS ||
//     "axone1k7s7clwgeycn6a5af476k6xytss9h9x6z7kdxql3u95fdn88kazs2js0hv";

export const DATAVERSE_ADDR =
    process.env.NEXT_PUBLIC_DATAVERSE_ADDR ||
    "axone19cq5ladm6mxu8clszem8d8cwj9haac4du08s5zq2uvpyg4ue625qqzm29g";

export const OBJECTARIUM_ADDR =
    process.env.NEXT_PUBLIC_OBJECTARIUM_ADDR ||
    "axone1fchxlkly99gqymnvpfdyqjw9xc2cx62yplvw3rcfl2t5wurj3j9sa7pn8y";

export const COGNITARIUM_ADDR =
    process.env.NEXT_PUBLIC_COGNITARIUM_ADDR ||
    "axone1dg5acs48rj4gezvruawvcy4y7yc4frt2v4an0dzux2dqcsq3ju0sqm62mw";

export const AXONE_DENOM = "uaxone";
export const AXON_GAS_PRICE = 0.025; // en AXONE_DENOM (uaxone) por unidad de gas
export const DEFAULT_GAS_LIMIT = 1_000_000;

//------------------------------------------------------------------
// MINIO S3

export const MINIO_USER =
    process.env.MINIO_USER ||
    "minioadmin";

export const MINIO_PASSWORD =
    process.env.MINIO_PASSWORD ||
    "minioadmin";

export const MINIO_ENDPOINT =
    process.env.MINIO_ENDPOINT ||
    "localhost:9000";

export const MINIO_PORT =
    process.env.MINIO_PORT ||
    "9000";

export const MINIO_USE_SSL =
    process.env.MINIO_USE_SSL !== undefined 

        ? process.env.MINIO_USE_SSL === "true"
            ? true
            : false
        : false;

export const MINIO_REGION =
    process.env.MINIO_REGION ||
    "us-east-1";

export const MINIO_BUCKET =
    process.env.MINIO_BUCKET ||
    "doctai";


//------------------------------------------------------------------
// SESSION

export const SESSION_MAX_AGE = 30 * 24 * 60 * 60; // 30 days

//------------------------------------------------------------------
// DB

export const swLogsDBDebug =
    process.env.STORAGE_POSTGRES_LOG_DEBUG !== undefined
        ? process.env.STORAGE_POSTGRES_LOG_DEBUG === "true"
            ? true
            : false
        : false;

//------------------------------------------------------------------
// UTILS

export const isBrowser = typeof window !== "undefined";


//------------------------------------------------------------------
// ENTITIES

export const ENTITY_NAMES = {
  Chat: "Chat",
  User: "User",
  ChatMessage: "ChatMessage",
};