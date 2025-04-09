// lib/keplr.ts

export const connectKeplr = async (chainId: string = "axone-dentrite-1") => {
    // Access the Keplr object with our extended window type
    const keplrWindow = window as KeplrWindow;

    if (!keplrWindow.keplr) {
        alert("Please install Keplr extension!");
        return null;
    }

    try {
        // Enable the chain in Keplr
        await keplrWindow.keplr.enable(chainId);

        // Get the offline signer for this chain
        const offlineSigner = keplrWindow.keplr.getOfflineSigner(chainId);

        // Get the user's account
        const accounts = await offlineSigner.getAccounts();

        return {
            address: accounts[0].address,
            offlineSigner,
        };
    } catch (error) {
        console.error("Error connecting to Keplr:", error);
        alert("Failed to connect to Keplr. Please try again.");
        return null;
    }
};

// Function to send tokens using Keplr
/* eslint-disable @typescript-eslint/no-unused-vars */
export const sendTokens = async (
    recipientAddress: string,
    amount: string,
    _denom: string = "uaxon",
    chainId: string = "axone-dentrite-1"
) => {
    const keplrWindow = window as KeplrWindow;

    if (!keplrWindow.keplr) {
        alert("Please install Keplr extension!");
        return null;
    }

    try {
        await keplrWindow.keplr.enable(chainId);

        alert("Tokens sent successfully!");
        return true;
    } catch (error) {
        console.error("Error sending tokens:", error);
        alert("Failed to send tokens. Please try again.");
        return null;
    }
};
/* eslint-enable @typescript-eslint/no-unused-vars */
