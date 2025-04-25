interface KeplrWindow extends Window {
    keplr?: Keplr;
}

interface Keplr {
    enable: (chainId: string) => Promise<void>;
    getOfflineSigner: (chainId: string) => OfflineSigner;
    getAccount: (
        chainId: string
    ) => Promise<{ address: string; pubKey: Uint8Array }>;
    signArbitrary: (
        chainId: string,
        signerAddress: string,
        data: string
    ) => Promise<{ signature: string; pub_key: { type: string; value: string } }>;
    getKey: (
        chainId: string
    ) => Promise<{
        name: string;
        algo: string;
        pubKey: Uint8Array;
        address: Uint8Array;
        bech32Address: string;
    }>;
}

interface OfflineSigner {
    getAccounts: () => Promise<{ address: string; pubKey: Uint8Array }[]>;
    signDirect: (
        signerAddress: string,
        signDoc: {
            bodyBytes: Uint8Array;
            authInfoBytes: Uint8Array;
            chainId: string;
            accountNumber: Long;
        }
    ) => Promise<{
        signed: {
            bodyBytes: Uint8Array;
            authInfoBytes: Uint8Array;
            chainId: string;
            accountNumber: Long;
        };
        signature: {
            signature: Uint8Array;
            pub_key: { type: string; value: string };
        };
    }>;
}
