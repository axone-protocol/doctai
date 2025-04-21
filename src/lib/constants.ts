//------------------------------------------------------------------
// COMSOS

export const CHAIN_ID = "axone-dentrite-1";

export const CONTRACT_ADDRESS_HEARTH_LABS = "wasm1v3xq2w5k6j8c4l7g3f5v3z";

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
