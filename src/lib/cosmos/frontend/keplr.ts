import { AXONE_NODE_RPC, CHAIN_ID } from "@/lib/constants";
import { CosmWasmClient } from "@cosmjs/cosmwasm-stargate";
import { fromBase64 } from "@cosmjs/encoding";
import { verifyADR36Amino } from "@keplr-wallet/cosmos";
import bs58 from "bs58";

export const getKeplr = (): Keplr => {
    // Check if the window object has the keplr property
    if (typeof window !== "undefined" && "keplr" in window) {
        const keplrWindow = window as KeplrWindow;
        if (!keplrWindow.keplr) {
            alert("Please install Keplr extension!");
            throw new Error("Keplr extension is not installed");
        } else {
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

export async function createTx(
    fromAddress: string,
    toAddress: string,
    amount: number
): Promise<string | null> {
    //   const { offlineSigner } = await connectKeplr();
    //   const client = await SigningStargateClient.connectWithSigner(AXONE_NODE_RPC, offlineSigner);

    //   const tx = await client.sendTokens(fromAddress, toAddress, [{ denom: "uaxone", amount: (amount * 1e6).toString() }], {
    //     amount: [{ denom: "uaxone", amount: "5000" }],
    //     gas: "200000",
    //   });

    //   return tx.transactionHash;

    return "11";
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
