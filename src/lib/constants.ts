//------------------------------------------------------------------
// AXONE

export const AXONE_NODE_RCP =
    process.env.NEXT_PUBLIC_AXONE_NODE_RCP ||
    "https://api.dentrite.axone.xyz:443/rpc";
export const AXONE_NODE_GRCP =
    process.env.NEXT_PUBLIC_AXONE_NODE_GRCP ||
    "axone-testnet-grpc.polkachu.com:17690";

//------------------------------------------------------------------
// COMSOS

export const CHAIN_ID = process.env.NEXT_PUBLIC_CHAIN_ID || "axone-dentrite-1";
export const HEARTH_LABS_ZONES_GOV_ADDRESS =
    process.env.NEXT_PUBLIC_HEARTH_LABS_ZONES_GOV_ADDRESS ||
    "axone1k7s7clwgeycn6a5af476k6xytss9h9x6z7kdxql3u95fdn88kazs2js0hv";

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