import { Window as KeplrWindow } from "@keplr-wallet/types";

declare global {
    interface Window extends KeplrWindow {}
}


// interface KeplrWindow extends Window {
//     keplr?: Keplr;
// }

// interface Keplr {
//     defaultOptions?: {
//         sign?: {
//             preferNoSetFee?: boolean;
//             preferNoSetMemo?: boolean;
//             disableBalanceCheck?: boolean;
//         };
//     };
//     enable: (chainId: string) => Promise<void>;

//     getOfflineSigner(
//         chainId: string,
//         signOptions?: KeplrSignOptions
//     ): OfflineAminoSigner & OfflineDirectSigner;

//     getOfflineSignerOnlyAmino(
//         chainId: string,
//         signOptions?: KeplrSignOptions
//     ): OfflineAminoSigner;

//     getOfflineSignerAuto(
//         chainId: string,
//         signOptions?: KeplrSignOptions
//     ): Promise<OfflineAminoSigner | OfflineDirectSigner>;

//     getAccount: (
//         chainId: string
//     ) => Promise<{ address: string; pubKey: Uint8Array }>;
//     signArbitrary: (
//         chainId: string,
//         signerAddress: string,
//         data: string
//     ) => Promise<{
//         signature: string;
//         pub_key: { type: string; value: string };
//     }>;
//     /**
//      * Keplr-specific function to sign arbitrary binary data
//      */
//     signBytes: (signerAddress: string, data: Uint8Array) => Promise<Uint8Array>;
//     getKey: (chainId: string) => Promise<{
//         name: string;
//         algo: string;
//         pubKey: Uint8Array;
//         address: Uint8Array;
//         bech32Address: string;
//     }>;
// }

// interface OfflineSigner {
//     getAccounts: () => Promise<{ address: string; pubKey: Uint8Array }[]>;
//     signDirect: (
//         signerAddress: string,
//         signDoc: {
//             bodyBytes: Uint8Array;
//             authInfoBytes: Uint8Array;
//             chainId: string;
//             accountNumber: bigint;
//         }
//     ) => Promise<{
//         signed: {
//             bodyBytes: Uint8Array;
//             authInfoBytes: Uint8Array;
//             chainId: string;
//             accountNumber: bigint;
//         };
//         signature: {
//             signature: string;
//             pub_key: { type: string; value: string };
//         };
//     }>;
// }

// interface OfflineDirectSigner {
//     readonly getAccounts: () => Promise<readonly AccountData[]>;
//     readonly signDirect: (
//         signerAddress: string,
//         signDoc: SignDoc
//     ) => Promise<DirectSignResponse>;
// }

// interface OfflineAminoSigner {
//     /**
//      * Get AccountData array from wallet. Rejects if not enabled.
//      */
//     readonly getAccounts: () => Promise<readonly AccountData[]>;
//     /**
//      * Request signature from whichever key corresponds to provided bech32-encoded address. Rejects if not enabled.
//      *
//      * The signer implementation may offer the user the ability to override parts of the signDoc. It must
//      * return the doc that was signed in the response.
//      *
//      * @param signerAddress The address of the account that should sign the transaction
//      * @param signDoc The content that should be signed
//      */
//     readonly signAmino: (
//         signerAddress: string,
//         signDoc: StdSignDoc
//     ) => Promise<AminoSignResponse>;
// }
