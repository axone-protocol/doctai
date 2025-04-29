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

export const HEARTH_LABS_ZONE_DID =
    process.env.NEXT_PUBLIC_HEARTH_LABS_ZONE_DID ||
    "did:key:zQ3shmDoRRiFmNzwDrnV2iPdgA9xaoAYsSq8GC9tGHsNL4R7a";

export const MINIO_DID =
    process.env.NEXT_PUBLIC_MINIO_DID ||
    "did:key:zQ3shgHvy6b16afQRDqyUb94xAhU7wTEqLPygqCFz1yE2KtMA";

export const HEARTH_LABS_ZONES_GOV_ADDRESS =
    process.env.NEXT_PUBLIC_HEARTH_LABS_ZONES_GOV_ADDRESS ||
    "axone1k7s7clwgeycn6a5af476k6xytss9h9x6z7kdxql3u95fdn88kazs2js0hv";

export const DATAVERSE_ADDR =
    process.env.NEXT_PUBLIC_DATAVERSE_ADDR ||
    "axone19cq5ladm6mxu8clszem8d8cwj9haac4du08s5zq2uvpyg4ue625qqzm29g";

export const OBJECTARIUM_ADDR =
    process.env.NEXT_PUBLIC_OBJECTARIUM_ADDR ||
    "axone1fchxlkly99gqymnvpfdyqjw9xc2cx62yplvw3rcfl2t5wurj3j9sa7pn8y";

export const COGNITARIUM_ADDR =
    process.env.NEXT_PUBLIC_COGNITARIUM_ADDR ||
    "axone1dg5acs48rj4gezvruawvcy4y7yc4frt2v4an0dzux2dqcsq3ju0sqm62mw";

//------------------------------------------------------------------
// SESSION

export const SESSION_MAX_AGE = 30 * 24 * 60 * 60; // 30 days

//------------------------------------------------------------------
// DB

export const swLogsDBDebug =
    process.env.LOGS_DB_DEBUG !== undefined
        ? process.env.LOGS_DB_DEBUG === "true"
            ? true
            : false
        : false;

//------------------------------------------------------------------
// UTILS

export const isBrowser = typeof window !== "undefined";


//------------------------------------------------------------------





