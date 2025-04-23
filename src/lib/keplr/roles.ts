import { CosmWasmClient } from "@cosmjs/cosmwasm-stargate";
import {
    AXONE_NODE_RCP,
    HEARTH_LABS_ZONES_GOV_ADDRESS,
} from "@/lib/constants";

export async function queryIsContributor(did: string): Promise<boolean> {
    try {
        const client = await CosmWasmClient.connect(AXONE_NODE_RCP);

        const query = {
            ask: {
                query: `tell('${did}', 'resource:register', Result, Evidence).`,
            },
        };

        const response = await client.queryContractSmart(
            HEARTH_LABS_ZONES_GOV_ADDRESS,
            query
        );

        const substitutions =
            response?.answer?.results?.[0]?.substitutions ?? [];

        const resultSubstitution = substitutions.find(
            (s: any) => s.variable === "Result"
        );

        const isPermitted = resultSubstitution?.expression === "permitted";

        console.log(
            `queryIsContributor → DID: ${did} → permitted: ${isPermitted}`
        );

        return isPermitted;
    } catch (error) {
        console.error(
            "queryIsContributor - Failed to check contributor permission:",
            error
        );
        return false;
    }
}
