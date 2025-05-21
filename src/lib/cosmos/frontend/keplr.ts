import { AXONE_NODE_RPC, CHAIN_ID } from "@/lib/constants";
import { CosmWasmClient } from "@cosmjs/cosmwasm-stargate";
import { fromBase64 } from "@cosmjs/encoding";
import { decodePubkey } from "@cosmjs/proto-signing";
import {
    QueryClient,
    setupAuthExtension,
    StargateClient,
} from "@cosmjs/stargate";
import { Tendermint34Client } from "@cosmjs/tendermint-rpc";
import { verifyADR36Amino } from "@keplr-wallet/cosmos";
import { Keplr, Window as KeplrWindow } from "@keplr-wallet/types";
import bs58 from "bs58";
import { BaseAccount } from "cosmjs-types/cosmos/auth/v1beta1/auth";

export const getKeplr = (): Keplr => {
    // Check if the window object has the keplr property
    if (typeof window !== "undefined" && "keplr" in window) {
        const keplrWindow = window as KeplrWindow;

        if (!keplrWindow.keplr) {
            alert("Please install Keplr extension!");
            throw new Error("Keplr extension is not installed");
        } else {
            keplrWindow.keplr.defaultOptions = {
                sign: {
                    preferNoSetFee: true,
                },
            };
            return keplrWindow.keplr;
        }
    }
    throw new Error("Keplr is only available in the browser context");
};

export const connectKeplr = async () => {
    // Access the Keplr object with our extended window type
    try {
        const keplr = getKeplr();
        // Enable the chain in Keplr
        await keplr.enable(CHAIN_ID);
        // Get the offline signer for this chain
        const offlineSigner = keplr.getOfflineSigner(CHAIN_ID);
        // Get the user's account
        const accounts = await offlineSigner.getAccounts();
        return {
            address: accounts[0].address,
            offlineSigner,
        };
    } catch (error) {
        if (error instanceof Error) {
            if (error.message.includes("modular chain info")) {
                alert(
                    "Keplr doesn't recognize the Axone network. Please check network configuration."
                );
            } else if (error.message.includes("Request rejected")) {
                alert(
                    "Connection request was rejected. Please try again and approve the connection."
                );
            } else {
                alert(`Failed to connect: ${error.message}`);
            }
        } else {
            alert("Failed to connect to Keplr. Please try again.");
        }
        console.error("Error connecting to Keplr:", error);
        return undefined;
    }
};

/**
 * Fetches account pubkey, sequence, and account number from chain
 */
export async function getAccountDetails(address: string) {
    console.log(
        `[getAccountDetails] Fetching on-chain account data for: ${address}`
    );

    const tmClient = await Tendermint34Client.connect(AXONE_NODE_RPC);
    const client = QueryClient.withExtensions(tmClient, setupAuthExtension);

    const accountAny = await client.auth.account(address);
    if (!accountAny) {
        throw new Error(`[getAccountDetails] Account not found for ${address}`);
    }

    const decoded = BaseAccount.decode(accountAny.value);

    if (!decoded.pubKey) {
        throw new Error(
            `[getAccountDetails] Public key not available for ${address}`
        );
    }

    const pubkey = decodePubkey(decoded.pubKey);

    if (!pubkey) {
        throw new Error(
            `[getAccountDetails] Public key not available for ${address}`
        );
    }

    const result = {
        pubkey,
        sequence: Number(decoded.sequence),
        accountNumber: Number(decoded.accountNumber),
    };

    console.log(
        `[getAccountDetails] Account details for ${address}:`,
        JSON.stringify(result, null, 2)
    );

    return result;
}

export function getDidFromPubKeyBase64(pubKeyBase64: string): string {
    // Decode base64 -> Uint8Array
    const pubKeyBytes = fromBase64(pubKeyBase64);

    // Multicodec prefix for secp256k1 pubkey: 0xe7 (decimal 0xE7 = 231)
    const multicodecHeader = Uint8Array.from([0xe7]); // secp256k1 pubkey
    const prefixedKey = new Uint8Array(
        multicodecHeader.length + pubKeyBytes.length
    );
    prefixedKey.set(multicodecHeader);
    prefixedKey.set(pubKeyBytes, multicodecHeader.length);

    // Encode to base58btc
    const didKey = `did:key:z${bs58.encode(prefixedKey)}`;

    console.log("getDidFromPubKeyBase64 - DID Key:", didKey);

    return didKey;
}

/**
 * Returns the wallet balance in AXONE from the given address
 */
export async function fetchWalletBalance(address: string): Promise<number> {
    try {
        const client = await CosmWasmClient.connect(AXONE_NODE_RPC);
        const balance = await client.getBalance(address, "uaxone");
        const value = parseFloat(balance.amount) / 1e6;
        console.log(`Wallet balance for ${address}: ${value} AXONE`);
        return value;
    } catch (error) {
        console.error("fetchWalletBalance - Error fetching balance:", error);
        return 0;
    }
}

export async function waitForTxConfirmation(
    txHash: string,
    retries = 10,
    delayMs = 2000
) {
    const client = await StargateClient.connect(AXONE_NODE_RPC);

    for (let i = 0; i < retries; i++) {
        const tx = await client.getTx(txHash);
        if (tx) return tx;
        console.log(
            `[ConfirmTx] Waiting for tx to confirm... (${i + 1}/${retries})`
        );
        await new Promise((resolve) => setTimeout(resolve, delayMs));
    }

    throw new Error(`Transaction ${txHash} not found after ${retries} retries`);
}

export function verifyADR36Signature({
    message,
    address,
    pubKeyBase64,
    signatureBase64,
}: {
    message: string;
    address: string;
    pubKeyBase64: string;
    signatureBase64: string;
}): boolean {
    return verifyADR36Amino(
        "axone", // Bech32 prefix only, not the full address
        address, // full address
        message,
        fromBase64(pubKeyBase64),
        fromBase64(signatureBase64),
        "secp256k1"
    );
}
