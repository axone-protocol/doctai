import { Secp256k1, Secp256k1Signature } from "@cosmjs/crypto";
import { sha256 } from "@cosmjs/crypto";
import { CHAIN_ID } from "./constants";
import { encodeSecp256k1Pubkey, verifyADR36Amino } from "@keplr-wallet/cosmos";
import { fromBase64 } from "@cosmjs/encoding";

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
