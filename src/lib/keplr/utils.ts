import { CosmWasmClient } from "@cosmjs/cosmwasm-stargate";
import {
    AXONE_NODE_RPC,
    COGNITARIUM_ADDR,
    DATAVERSE_ADDR,
    HEARTH_LABS_ZONES_GOV_ADDRESS,
} from "@/lib/constants";

export async function queryIsContributor(did: string): Promise<boolean> {
    try {
        const client = await CosmWasmClient.connect(AXONE_NODE_RPC);

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

// Método independiente para obtener el Governance Address usando el DID de la zona
export async function fetchZoneGovAddress(zoneDID: string): Promise<string> {
    try {
        const client = await CosmWasmClient.connect(AXONE_NODE_RPC);

        const query = {
            select: {
                query: {
                    prefixes: [],
                    select: [
                        { variable: "credential" },
                        { variable: "p" },
                        { variable: "o" }
                    ],
                    where: {
                        lateral_join: {
                            left: {
                                bgp: {
                                    patterns: [
                                        {
                                            subject: { variable: "credential" },
                                            predicate: { named_node: { full: "dataverse:credential:body#subject" } },
                                            object: { node: { named_node: { full: zoneDID } } }
                                        },
                                        {
                                            subject: { variable: "credential" },
                                            predicate: { named_node: { full: "dataverse:credential:body#type" } },
                                            object: { node: { named_node: { full: "https://w3id.org/axone/ontology/v4/schema/credential/governance/text/GovernanceTextCredential" } } }
                                        },
                                        {
                                            subject: { variable: "credential" },
                                            predicate: { variable: "dataverse:credential:body#claim" },
                                            object: { variable: "claim" }
                                        },
                                        {
                                            subject: { variable: "claim" },
                                            predicate: { named_node: { full: "https://w3id.org/axone/ontology/v4/schema/credential/governance/text/isGovernedBy" } },
                                            object: { variable: "governance" }
                                        }
                                    ]
                                }
                            },
                            right: {
                                bgp: {
                                    patterns: [
                                        {
                                            subject: { variable: "governance" },
                                            predicate: { variable: "p" },
                                            object: { variable: "o" }
                                        }
                                    ]
                                }
                            }
                        }
                    }
                }
            }
        };

        const response = await client.queryContractSmart(COGNITARIUM_ADDR, query);

        const bindings = response?.results?.bindings;

        if (!bindings || bindings.length === 0) {
            throw new Error("No governance bindings found for the provided DID");
        }

        // Filtramos solo bindings de fromGovernance
        const governanceBindings = bindings.filter(
            (binding: any) =>
                binding.p?.value?.full === "https://w3id.org/axone/ontology/v4/schema/credential/governance/text/fromGovernance"
        );

        if (governanceBindings.length === 0) {
            throw new Error("No fromGovernance fields found in the bindings");
        }

        // Ordenamos por credential ID, alfabéticamente (mayor = más reciente)
        governanceBindings.sort((a: any, b: any) => {
            const credA = a.credential?.value?.full || "";
            const credB = b.credential?.value?.full || "";
            return credB.localeCompare(credA); // credB - credA para que el más grande quede primero
        });

        const lastGovernance = governanceBindings[0];

        const fromGovernanceURI = lastGovernance.o?.value?.full;

        if (typeof fromGovernanceURI !== "string") {
            throw new Error("Invalid fromGovernance value format");
        }

        const matches = fromGovernanceURI.match(/cosmwasm:law-stone:([^?]+)/);
        if (!matches) {
            throw new Error("Unable to parse law-stone address");
        }

        return matches[1]; // El governance address final limpio
    } catch (error) {
        console.error("fetchZoneGovAddress error:", error);
        return Promise.reject(error);
    }
}


// Devuelve el código Prolog asociado a un contrato Law Stone
export async function fetchPrologFromAddress(contractAddress: string): Promise<string> {
    try {
        const client = await CosmWasmClient.connect(AXONE_NODE_RPC);

        const query = {
            program_code: {}
        };

        const response = await client.queryContractSmart(contractAddress, query);

        const base64Encoded = response;
        if (!base64Encoded || typeof base64Encoded !== "string") {
            throw new Error("Invalid base64 program_code response");
        }

        // Decodificamos el base64 a texto
        const decoded = Buffer.from(base64Encoded, "base64").toString("utf-8");
        return decoded;
    } catch (error) {
        console.error("fetchPrologFromAddress error:", error);
        return Promise.reject("Failed to fetch or decode Prolog program");
    }
}